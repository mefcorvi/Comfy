Repository =
{
    registerEntityType: function (entityType) {
        this.__cache[entityType] = {};

        this['Load' + entityType] = (function (p) {
            return function (id, onSuccess, context) {
                this.Load(p, id, onSuccess, context);
            };
        })(entityType);

        this['Filter' + entityType] = (function (p) {
            return function (filter, onSuccess, context) {
                this.Filter(p, filter, onSuccess, context);
            };
        })(entityType);

        Auto.Events(Repository, [
            entityType.toCamelCase() + 'Saved',
            entityType.toCamelCase() + 'Loaded',
            entityType.toCamelCase() + 'Deleted'
        ]);
    },

    __cache: {},

    _addToCache: function(entity) {
        var entityTypeName = entity.get_type().get_name();
        var id = entity.get_id();

        Repository.__cache[entityTypeName][id] = entity;
    },

    Abort: function(rid) {
        if (Repository._dataService.abort(rid)) {
            Repository.raise_processed();
        }
    },

    Load: function (entityTypeName, id, onSuccess, context) {
        Repository.raise_processing();

        this._dataService.invoke(entityTypeName, 'Load',
            function (dto) {
                if (dto) {
                    var entity = this._dtoProcessor.toEntity(dto, true);
                    this._raiseEntityEvent(entityTypeName, [entity], 'Loaded', context);
                }

                if (onSuccess)
                    onSuccess(entity);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), { id: id });
    },

    _reloadQueue: new QueueProcessor(function (sender, args) {
        var data = {};
        var ids = [];
        var entityTypeName = args.queueId;

        for (var i = 0; i < args.data.length; i++) {
            var entityId = args.data[i].entity.get_id() * 1;
            data[entityId] = args.data[i];
            ids.add(entityId);
        }

        if (ids.length == 0) {
            return;
        }

        Repository._dataService.invoke(entityTypeName, 'Filter', function (dto) {
            var result = [];

            for (var i = 0; i < dto.length; i++) {
                var entity = data[dto[i]._id * 1].entity;
                var callback = data[dto[i]._id * 1].callback;

                this._dtoProcessor.copyFromDTO(entity, dto[i], true);

                if (callback)
                    callback(entity);
            }

            Repository.raise_processed();
        } .bind(Repository), function (error) {
            Repository.raise_processed();
            Repository.raise_error(error);
        }, {
            filter: "Id IN (" + ids.join(',') + ")",
            searchQuery: null,
            orderBy: null,
            page: null,
            pageSize: null
        });
    }),

    Reload: function (entity, onSuccess) {
        var entityTypeName = entity.rootType;
        var id = entity.get_id();

        this._reloadQueue.add(entityTypeName, { entity: entity, callback: onSuccess });
    },

    _deepLoadQueue: new QueueProcessor(function (sender, args) {
        var data = {};
        var ids = [];
        var entityTypeName = args.queueId;

        for (var i = 0; i < args.data.length; i++) {
            var entityId = args.data[i].entity.get_id() * 1;

            if (isNaN(entityId)) {
                throw new Error('Wrong identifier of entity');
            }

            if (!data[entityId]) {
                data[entityId] = [];
                ids.add(entityId);
            }

            data[entityId].add(args.data[i]);
        }

        if (ids.length == 0) {
            return;
        }

        Repository.raise_processing();

        Repository._dataService.invoke(entityTypeName, 'BatchDeepLoad', function (dto) {
            for (var i = 0; i < dto.length; i++) {
                var dtoId = dto[i]._id * 1;
                var entity = data[dtoId][0].entity;

                this._dtoProcessor.copyFromDTO(entity, dto[i], true);

                for (var j = 0; j < data[dtoId].length; j++) {
                    var callback = data[dtoId][j].callback;

                    if (callback)
                        callback(entity);
                }
            }

            Repository.raise_processed();
        } .bind(Repository), function (error) {
            Repository.raise_processed();
            Repository.raise_error(error);
        }, {
            ids: ids
        });
    }),

    DeepLoad: function (entity, onSuccess) {
        var entityTypeName = entity.rootType;
        var id = entity.get_id() * 1;

        if (isNaN(id)) {
            throw new Error('Wrong identifier of entity');
        }

        this._deepLoadQueue.add(entityTypeName, { entity: entity, callback: onSuccess });
    },

    Save: function (data, onSuccess, context, entityTypeName, deepLevel) {
        var batchSave = data instanceof Array;

        if (batchSave && data.length === 0)
            throw new Error('Trying to save empty collection');

        Repository.raise_processing();

        var entityTypeName = isNullOrUndefined(entityTypeName) ? (batchSave ? data[0].get_type().get_name() : data.get_type().get_name()) : entityTypeName; // TODO: there is may be problem when batch contains entities of different types

        this._dataService.invoke(entityTypeName, batchSave ? 'BatchSave' : 'Save',
            function (dto) {
                var result = [];

                var processFunc = function (entity, dto) {
                    this._dtoProcessor.copyFromDTO(entity, dto, true);
                    Repository._addToCache(entity);

                    result.add(entity);
                } .bind(this);

                if (batchSave) {
                    for (var i = 0; i < dto.length; i++)
                        processFunc(data[i], dto[i]);
                } else
                    processFunc(data, dto);

                this._raiseEntityEvent(entityTypeName, result, 'Saved', context);

                if (onSuccess)
                    onSuccess(data, context);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), batchSave ? { entities: data} : { entity: data }, isDefined(deepLevel) ? deepLevel : 1);
    },

    __raiseDeletedEvent: function (entity, context) {
        if (isFunction(entity.onDeleting)) {
            entity.onDeleting();
        }

        //++110310_Aykaev, raise deleted event for dependent sets
        if (entity.__dependentSets && entity.__dependentSets.length > 0) {
            for (var i = 0; i < entity.__dependentSets.length; i++) {
                var setName = entity.__dependentSets[i];
                var dependentEntities = entity['get_' + setName]();

                for (var j = dependentEntities.length - 1; j >= 0; j--) {
                    this.__raiseDeletedEvent(dependentEntities[j], context);
                }
            }
        }
        //--110310_Aykaev

        entity.dispose();
        var entityTypeName = entity.get_type().get_name();
        Repository._removeFromCache(entityTypeName, entity._id);

        this._raiseEntityEvent(entityTypeName, [entity], 'Deleted', context);
    },

    Delete: function (entity, onSuccess, context) {
        Repository.raise_processing();

        this._dataService.invoke(entity.get_type().get_name(), 'Delete',
            function () {
                this.__raiseDeletedEvent(entity, context);

                if (onSuccess)
                    onSuccess();

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), { entity: entity }, 0);
    },

    DeleteById: function (entityType, id, onSuccess, context) {
        Repository.raise_processing();

        this._dataService.invoke(entityType, 'DeleteById',
            function () {                
                if (onSuccess)
                    onSuccess();

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), { id: id }, 0);
    },

    Filter: function (entityTypeName, args, onSuccess, context) {        
        Repository.raise_processing();

        return this._dataService.invoke(entityTypeName, 'Filter',
            function (dto) {
                var result = [];

                for (var i = 0; i < dto.length; i++) {
                    var entity = this._dtoProcessor.toEntity(dto[i]);
                    result.add(entity);
                }

                this._raiseEntityEvent(entityTypeName, result, 'Loaded', context);

                if (onSuccess)
                    onSuccess(result, context);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this),
                typeof args === 'string' ?
                {
                    filter: args,
                    searchQuery: null,
                    orderBy: null,
                    page: null,
                    pageSize: null
                } :
                args
            );
    },

    Count: function (entityTypeName, args, onSuccess) {
        Repository.raise_processing();

        this._dataService.invoke(entityTypeName, 'Count',
            function (result) {
                if (onSuccess)
                    onSuccess(result);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), args);
    },

    GetCached: function (entityTypeName, id) {
        return this.__cache[entityTypeName][id];
    },

    _removeFromCache: function(entityTypeName, id) {
        delete this.__cache[entityTypeName][id];
    },

    FilterCached: function (entityTypeName, filter) {
        var result = [];

        var storage = this.__cache[entityTypeName];

        for (var prop in storage) {
            if (typeof storage[prop] === 'function')
                continue;

            if (filter(storage[prop]))
                result.add(storage[prop]);
        }

        return result;
    },

    _getQueue: new QueueProcessor(function (sender, args) {
        var queue = args.data;
        var entityTypeName = args.queueId;

        var notFounded = [];

        for (var i = 0; i < queue.length; i++) {
            notFounded.add(queue[i].notFounded);
        }

        if (notFounded.length === 0) {
            return;
        }

        Repository.Filter(entityTypeName, "Id In (" + notFounded.distinct().join(',') + ")", function (result) {
            for (var i = 0; i < queue.length; i++) {
                var callbackResult = [];

                for (var j = 0; j < result.length; j++) {
                    if (queue[i].notFounded.contains(result[j]._id)) {
                        callbackResult.add(result[j]);
                    }
                }

                if (queue[i].founded) {
                    callbackResult.add(queue[i].founded);
                }

                if (queue[i].callback) {
                    queue[i].callback(queue[i].singleMode
                                        ?
                                        (callbackResult.length > 0 ?
                                          callbackResult[0] : undefined)
                                        :
                                        callbackResult);
                }
            }
        });
    }),

    __Get: function (entityTypeName, id, callback, throwException) {
        if (isNullOrUndefined(id)) {
            throw new Error('Id is null or undefined');
        }

        var singleMode = !(id instanceof Array);

        if (singleMode) {
            id = [id];
        }

        var notFounded = [];
        var founded = [];

        for (var i = 0; i < id.length; i++) {
            var itemId = id[i]*1;
            var found = this.__cache[entityTypeName][itemId];

            if (found) {
                founded.add(found);
            } else {
                notFounded.add(itemId);
            }
        }

        if (notFounded.length == 0) {
            callback(singleMode ? founded[0] : founded);
            return;
        }

        var callbackWrapper = callback;
        
        if(throwException) {
            callbackWrapper = function(result) 
            {  
                if(result != null) {
                    callback(result);
                } else {
                    throw new Error('Cannot find \'' + entityTypeName + '\' with id: \'' + id + '\'');
                }
            };
        }

        this._getQueue.add(entityTypeName, {
            singleMode: singleMode,
            notFounded: notFounded.distinct(),
            founded: founded,
            callback: callbackWrapper
        });
    },

    Get: function(entityTypeName, id, callback) {
        this.__Get(entityTypeName, id, callback, true);
    },

    GetOrDefault: function(entityTypeName, id, callback) {
        this.__Get(entityTypeName, id, callback, false);
    },

    Clear: function () {
        for (var prop in this.__cache) {
            if (typeof this.__cache[prop] === 'function')
                continue;

            for (var p in this.__cache[prop]) {
                if (typeof this.__cache[prop][p] === 'function')
                    continue;

                delete this.__cache[prop][p];
            }
        }
    },

    Validate: function (data, onSuccess, onError, deepLevel) {
        Repository.raise_processing();

        var isBatch = data instanceof Array;

        if (isBatch) {
            throw new Error("Batch validation isn't supported.");
        }

        var entityTypeName = data.get_type().get_name();

        this._dataService.invoke(entityTypeName, "Validate",
            function (result) {
                if (result && result.length > 0 && onError) {
                    var processedResult = [];

                    for (var i = 0; i < result.length; i++) {
                        processedResult.add(result[i].__type ? this._dtoProcessor.toEntity(result[i]) : result[i]);
                    }

                    onError(processedResult);
                }

                if ((!result || !result.length) && onSuccess)
                    onSuccess();

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);

                if (onError)
                    onError(error);
            } .bind(this), { data: data }, isDefined(deepLevel) ? deepLevel : 1);
    },

    ValidateAndSave: function (data, onSuccess, onError, context) {
        Repository.Validate(data, function () {
            Repository.Save(data, onSuccess, context);
        }, function (error) {
            if (onError) {
                onError(error)
            }
        });
    },

    Method: function (entityTypeName, methodName, params, onSuccess) {
        Repository.raise_processing();

        this._dataService.invoke(entityTypeName, methodName,
            function (result) {
                if (result && result.__type) {
                    result = this._dtoProcessor.toEntity(result);
                    this._raiseEntityEvent(entityTypeName, [result], 'Loaded');
                }
                else if (result instanceof Array && result.length > 0 && result.first().__type) {
                    result = result.select(function (item) { return this._dtoProcessor.toEntity(item); }, this);
                    this._raiseEntityEvent(entityTypeName, result, 'Loaded');
                }

                if (onSuccess)
                    onSuccess(result);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), params);
    },

    _raiseEntityEvent: function (entityTypeName, entities, eventName, context) {
        if (entities.length > 0)
            this['raise_' + entityTypeName.toCamelCase() + eventName]({ entities: entities }, context);
    }
};

Auto.Properties(Repository, [
    'dtoProcessor',
    'dataService'
]);

Auto.Events(Repository, [
    'processing',
    'processed',
    'error'
]);

Repository.set_dtoProcessor(new DTOProcessor());
Repository.set_dataService(Services.DataService);