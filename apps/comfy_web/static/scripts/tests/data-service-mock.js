var dtoProcessorMock = {
    toEntity: function(obj) {return obj;},
    copyFromDTO: function(entity, dto, attach) {
        if (this._copyFromDtoAction) {
            return this._copyFromDtoAction(entity, dto, attach);
        }
    },

    set_copyFromDtoAction: function(func) {
        this._copyFromDtoAction = func;
    }
};

var dataServiceMock = {
    invoke: function(entityTypeName, methodName, onSuccess, onError, args) {
        var result = null;
        
        switch(methodName) {
            case 'Load': {
                if(this._loadAction) {
                    onSuccess(this._loadAction(args));
                } else {            
                    var entity = eval('(new ' + entityTypeName + ')');
                    entity.set_id(args.id);
                    
                    onSuccess(entity);
                }
                break;
            }
            case 'Save': {
                onSuccess(args.entities);
                break;
            }
            case 'BatchSave': {
                onSuccess(args.entities);
                break;
            }
            case 'BatchDeepLoad': {
                if (this._deepLoadAction) {
                    onSuccess(this._deepLoadAction(args));
                } else {
                    var entities = [];

                    for (var i = 0; i < args.ids.length; i++) {
                        var entity = eval('(new ' + entityTypeName + ')');
                        entity.set_id(args.ids[i]);
                        entities.add(entity);
                    }
                    
                    onSuccess(entities);
                }

                break;
            }
            case 'Delete': {
                onSuccess();
                break;
            }
            case 'Filter': {
                if(this._filterAction)
                    onSuccess(this._filterAction(args, entityTypeName));                
                
                break;                
            }
        }
    },
    
    set_filterAction: function(func) {
        this._filterAction = func;
    },
    
    set_loadAction: function(func) {
        this._loadAction = func;
    },

    set_deepLoadAction: function(func) {
        this._deepLoadAction = func;
    }
};