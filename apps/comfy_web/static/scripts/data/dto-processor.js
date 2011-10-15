var DTOProcessor = function() {

};

Object.extend(DTOProcessor.prototype, {
    toEntity: function(dto, forceRewrite) {
        if (forceRewrite) {
            dto.$__forceRewrite = true;
        }
        
        return this._getEntityFrom(dto);
    },

    toEntityCollection: function(dto, forceRewrite) {
        var convertedEntities = [];        
        for (var i = 0; i < dto.length; i++) {
            convertedEntities.add(this.toEntity(dto[i]));
        }    

        return convertedEntities;
    },

    getEntityType: function(dto) {
        var match = dto.__type.match(/^(\w+):#/);
        if (match.length < 2)
            return null;

        return Type.getType(match[1]);
    },

    _getEntityFrom: function(dto, attachingInstance) {
        var type = this.getEntityType(dto);
        var entityTypeName = type.get_name();

        var entity;
        
        if (dto._id)
            entity = Repository.GetCached(entityTypeName, dto._id) || attachingInstance;

        if (!entity)
            entity = this._createEntity(dto);
        else if (entity._version !== dto._version || dto.$__forceRewrite)
            this.copyFromDTO(entity, dto);

        if (dto._id)
            Repository._addToCache(entity);

        return entity;
    },

    _createEntity: function(dto) {        
        var typeName = this.getEntityType(dto).get_name();
        var entity = eval('(new ' + typeName + '())');

        this.copyFromDTO(entity, dto);

        return entity;
    },

    copyFromDTO: function(entity, dto, attachToRepository) {
        for (var property in dto) {
            if (property === '__type' || property === '_guid' || property === '$__forceRewrite')
                continue;

            var dtoProperty = dto[property];

            var setterName = entity['set' + property] ? 'set' + property : '_set' + property;

            if (!entity[setterName] || typeof (entity[setterName]) !== 'function')
                throw new Error('Can\'t copy field ' + property + ' from DTO in class ' + entity.get_type().get_name() + '. Setter not found.');
            else if (dtoProperty && dtoProperty.__type && Tracking.isTrackingDto(dtoProperty)) {
                this._processTrackingCollection(entity['get' + property](null, false), dto[property], attachToRepository);
            }
            else if (dtoProperty instanceof Array) {
                var target = entity['get' + property]();
                if (!target)
                    throw new Error('Array ' + property + ' in ' + entity.get_type().get_name() + ' must be initialized in constructor.')

                this._processArray(target, dto[property]);
            }
            else if (dtoProperty && dtoProperty.__type) {
                var converted = this._getEntityFrom(dto[property], attachToRepository ? entity['get' + property]() : null);

                entity[setterName](converted);
            }
            else if (entity.__enums[property]) {
                entity[setterName](dto[property] !== null ? entity.__enums[property].withValue(dto[property]) : null);
            }
            else if (entity.__dates[property]) {
                entity[setterName](Date.parseWcf(dto[property]));
            }
            else if (dtoProperty == null && entity['get' + property](null, false) && entity['get' + property](null, false).__isEntitySet) {
            }
            else
                entity[setterName](dtoProperty);
        }
    },

    _processTrackingCollection: function(target, sourceDto, attachItems) {        
        var converted = [];
        
        if(target.__isDataSource || target.__isEntitySet) {
            converted = sourceDto.items.select(function(item) {
                var attachingInstance = null;
                if (attachItems) {
                    attachingInstance = target.items.singleOrDefault(function(targetItem) {
                        return targetItem._id === item._id || targetItem._guid === item._guid;
                    }) || target.added.singleOrDefault(function(targetItem) {
                        return targetItem._id === item._id;
                    });
                }

                return this._getEntityFrom(item, attachingInstance);
            }, this);
        } else if (target.__isEnumCollection) {
            converted = sourceDto.items.select(function(item) {
                return target.__enumType.withValue(item);
            });
        } else
            converted = sourceDto.items;
        
        converted.forEach(function(item) {
            if(!target.items.contains(item) && !target.removed.contains(item))
                target.add(item, {notTrackable: true});
                
            if(target.added.contains(item))
                target.added.remove(item);
        });
        
        target.removed.remove(target.removed.where(function(item) {
            return !converted.contains(item);
        }));
        
        target.remove(target.items.where(function(item) {
            return !converted.contains(item) && !target.added.contains(item);
        }), {notTrackable: true});
    },

    _processArray: function(target, sourceDto, attachItems, args) {    
        if (sourceDto.length === 0) {
            target.clear();
            return;
        }
        if (target.__isDataSource || target.__isEntitySet) {
            var converted = sourceDto.select(function(item) {
                var attachingInstance = null;
                if (attachItems) {
                    attachingInstance = target[sourceDto.indexOf(item)]
                }

                return this._getEntityFrom(item, attachingInstance);
            }, this);

            target.synchronize(converted, null, args);
        }
        else if (target.__isEnumCollection) {
            var converted = sourceDto.select(function(item) {
                return target.__enumType.withValue(item);
            });

            target.synchronize(converted, null, args);
        }
        else {
            target.synchronize(sourceDto, null, args);
        }
    }
}); 