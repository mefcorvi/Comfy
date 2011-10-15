mockRepository = function() {
    Repository.set_dtoProcessor(dtoProcessorMock);
    Repository.set_dataService(dataServiceMock);
    
    Repository.__get = Repository.Get;
    Repository.Get = function(entityTypeName, id, callback) {
        Repository.Filter(entityTypeName, "Id In (" + ((id instanceof Array) ? id.join(',') : id) + ")", callback);
    }
};

restoreRepository = function() {
    Repository.Clear();    
    Repository.set_dtoProcessor(new DTOProcessor());
    Repository.set_dataService(Services.DataService);
    
    dataServiceMock.set_filterAction(null);
    dataServiceMock.set_loadAction(null);
    dataServiceMock.set_deepLoadAction(null);

    dtoProcessorMock.set_copyFromDtoAction(null);
    
    Repository.Get = Repository.__get;
    Repository._deepLoadQueue.clear();
    delete Repository.__get;
};