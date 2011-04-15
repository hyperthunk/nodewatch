//
// Erlang System Monitoring Dashboard: Backbone Application Module
//
// Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

CollectionView = Backbone.View.extend({
    debuggerTag: 'CollectionView',
    initialize: function() {
        _(this).bindAll('render');
        this.collection.bind('change', this.render);
        this.collection.bind('add', this.render);
        this.collection.bind('remove', this.render);
        this.collection.bind("refresh", this.render);
    },
    render: function() {
        console.debug("rendering " + this.debuggerTag);
        if (this.directives === undefined) {
            throw {
                message: "Unable to render"
            }
        } else {
            // TODO: consider keeping the templates on the server and $.load them
            var domId = this.el.attr('id');
            var templateClass = '.' + domId + '-template';
            this.el = 
                $(this.el)
                    .html($(templateClass).html())
                    .removeClass(templateClass)
                    .attr('id', domId)
                    .directives(this.directives)
                    .render({elements: this.collection.toJSON()});
        }
    }
});

NodeListView = CollectionView.extend({
    debuggerTag: 'NodeListView',
    directives: {
        //trigger a loop
        'dd' : {
            'el<-elements' : { 
                'a@href+': 'el.id',
                'a': 'el.id'
            }
        }
    }
});

LoginView = Backbone.View.extend({
    debuggerTag: 'LoginView',
    model: Session,
    events: {
        "click input.loginButton" : "login"
    },
    initialize: function() {
        _.bindAll(this, 'render', 'hide', 'login');
        // this.model.bind('change:connected', this.toggle);
        // this certainly isn't the only view for a sesion.
        // this.model.view = this;
        Service.loadFragment('static/login.html', this.el);
    },
    render: function() {
        //this.el.dialog({ autoOpen: false });
        this.el.show();
        return this;
    },
    hide: function() {
        this.el.hide();
        return this;
    },
    login: function() {
        if (this.$('form').validate()) {
            var uname = this.$('#username').val();
            var pwd = this.$('#password').val();
            this.model.set({
                username: uname,
                password: pwd
            });
            if (this.model.login()) {
                Notify.ok('Login Succeeded - Establishing Websocket');
                return true;
            } else {
                Notify.grumble('Login Failed for ${n}', {n: uname});
                this.$('#login-info')
                    .removeClass('ui-state-highlight')
                    .addClass("ui-state-error");
                return false;
            }
        }
    }
});

// TODO: seperate into Application[Model] and ApplicationView

ApplicationView = Backbone.View.extend({
    debuggerTag: 'ApplicationView',
    model: Session,
    templates: {
        subscription: _.template('/service/subscriptions/${username}')
    },
    initialize: function() {
        _.bindAll(this, 'render', 'handleConnected', 'refreshData');
        this.model.bind('change:connected', this.handleConnected);
        this.model.bind('websock:data', function(ev) { 
            console.debug('got data from websocket.....');
        });
        this.model.view = this;

        var loginPanel = $('#loginPanel');
        this.loginView = new LoginView({el: loginPanel, model: this.model});
        this.el.tabs();

        this.nodes = new NodeSet();
        this.nodeListView = new NodeListView({
            el: $('#navbar-node'),
            collection: this.nodes
        });
    },
    render: function() {
        var session = arguments.length >= 1 ? arguments[0] : this.model;
        if (session.get('connected') == true) {
            console.debug("Session Already Established - rendering app.");
            this.loginView.hide();
            this.el.show();
            this.refreshData();
        } else if (session.hasCookie()) {
            console.debug("Session Cookie Present - attempting " +
                          "to re-authenticate.");
            if (!session.login()) {
                console.debug("Session Cookie Invalid " +
                              "- User Login Required.");
                this.loginView.render();
            }
        } else {
            console.debug("No Session - User Login Required.");
            this.loginView.render();
        }
        return this;
    },
    refreshData: function() {
        var username = this.model.get('username');
        this.nodes.fetch();
        this.subscriptions = new SubscriptionList([], {
            url: '/service/subscriptions/' + username
        });
    },
    handleConnected: function(_, connected) {
        if (connected) {
            this.render();
        } else {
            Notify.grumble('WebSocket connection broken!\n' +
                           'Attempting to re-establish session....');
            $.cookie("sid", null);
            if (!this.loginView.render().login()) {
                Notify.show("Unable to establish session!");
            }
        }
        return this;
    }
});

