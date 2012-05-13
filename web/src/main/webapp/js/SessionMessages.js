jQuery(document).ready(function () {
    var SessionMessage = Backbone.Model.extend({
        idAttribute: "stamp",
        defaults: function() {
            return {
                stamp: 0,
                severity: "message_minor",
                text: "Default message"
            };
        }
    });

    var SessionMessageList = Backbone.Collection.extend({
        url: "/elw/rest/SessionMessage",  //  TODO base drifts along the page URLs, backbone Router helps here
        model: SessionMessage
    });

    var sessionMessageList = new SessionMessageList();

    var SessionMessageView = Backbone.View.extend({
        tagName: "li",
        events: {
            "click a.destroy" : "destroy"
        },

        initialize: function() {
            this.model.bind('change', this.render, this);
            this.model.bind('destroy', this.remove, this);
        },

        render: function() {
            this.$el.html(this.model.get('text'));
            jQuery(this.$el).toggleClass(this.model.get('severity'));
            return this;
        },

        destroy: function() {
            this.model.destroy();
        }
    });

    var SessionMessageListView = Backbone.View.extend({
        el: jQuery("#SessionMessageList_target"),
        events: {
            "click #SessionMessageList_clearAll": "clearAll"
        },
        initialize: function() {
            this.clearAllElem = this.$('#SessionMessageList_clearAll');

            sessionMessageList.bind('add', this.addOne, this);
            sessionMessageList.bind('reset', this.addAll, this);
            sessionMessageList.bind('all', this.render, this);

            sessionMessageList.fetch();
        },
        render: function() {
            var messageCount = sessionMessageList.length;

            if (messageCount > 0) {
                this.clearAllElem.show();
            } else {
                this.clearAllElem.hide();
            }
        },
        addOne: function(sessionMessage) {
            var view = new SessionMessageView({model: sessionMessage});
            this.$("#SessionMessageList_messageArea").append(view.render().el);
        },
        addAll: function() {
            sessionMessageList.each(this.addOne);
        },
        clearAll: function() {
            sessionMessageList.each(function(view) {
                view.destroy();
            });
        }
    });

    var sessionMessageListView = new SessionMessageListView();
});
