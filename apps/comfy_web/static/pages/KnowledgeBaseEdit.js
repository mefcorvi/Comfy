({
    title: "Knowledge base",
    cssClass: 'knowledgeEdit_page',
    
    data: [
        'page'
    ],
    
    onLoad: function() {
        var pageId = this.get_param('pageId') * 1;

        if (pageId) {
            Repository.Load('HelpPage', pageId, function(result) {
                this.loadPage(result);
            }.bind(this));
        }
    },

    customFunctions: {
        'saveData': function(callback) {
            var page = this.get_data().get_page();
            
            if (!page) {
                page = new HelpPage();
                page.set_isUnread(false);
            }

            page.set_title(this.pageTitle.get_text());
            page.set_content(this.textEditor.get_text());

            Repository.Save(page, callback);
        },
        'loadPage': function(page) {
            this.get_data().set_page(page);
            
            if (page) {
                this.pageTitle.set_text(page.get_title());
                this.textEditor.set_text(page.get_content());
            }
        }
    },
    
    controls: [
        {
            id: 'pageTitle',
            type: 'textBox',
            watermark: 'Page title',
            width: '100%',
            height: '34px',
            cssClass: 'knowledge_page_title'
        },
        {
            id: 'textEditor',
            type: 'textEditor',
            width: '*',
            height: '*'
        }
    ]
})