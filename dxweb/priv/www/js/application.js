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
$(document).ready(function() {

    _.templateSettings = {
      interpolate : /\$\{(.+?)\}/g,
      evaluate:     /#\{(.+?)\}/g
    };

    window.Notify = {
        // TODO: implicitly pass on the arguments instead of this duplication...
        ok: function() {
            var text = arguments[0];
            if (arguments.length > 1) {
                text = _.template(text, arguments[1]);
            }
            return this.show(text, 'success-notice');
        },
        grumble: function() {
            var text = arguments[0];
            if (arguments.length > 1) {
                text = _.template(text, arguments[1]);
            }
            return this.show(text, 'error-notice');
        },
        show: function(text, type) {
            var text = arguments[0];
            if (arguments.length > 1) {
                text = _.template(text, arguments[1]);
            }
            return $.noticeAdd({
                text: text,
                stay: false,
                type: type || 'notice'
            });
        }
    };

    window.NoOp = function() {};

    window.Service = {
        debuggerTag: 'Service',
        loadFragment: function(path, dest) {
            return this.get({url: path,
                             accept: 'application/html',
                             dataType: 'html'})
                       .success(function(html) { dest.html(html); });
        },

        get: function(opts) {
            if (typeof(opts) == "string") {
                return this.http({ url: opts });
            } else {
                console.debug('opts:');
                console.debug(opts);
                return this.http(opts);
            }
        },

        postForm: function(url, data) {
            return $.ajax({
                async: false,
                url: url,
                type: 'POST',
                data: data
            });
        },

        http: function(opts) {
            return $.ajax(
                _.defaults(opts, {
                    async: true,
                    type: 'GET',
                    contentType: 'application/json',
                    crossDomain: true,
                    processData: false,
                    dataType: 'json',
                }));
        }
    };

    window.Subscription = Backbone.Model.extend({
        debuggerTag: 'Subscription',
        defaults: {
            id: 'user.node.sensor',
            user: 'user',
            node: 'nonode@nohost',
            sensor: 'none',
            mode: 'instrument'
        },
        parse: function(response) {
            var data = response.subscription;
            data.id = _.template('${user}-${node}-${sensor}', data);
            return data;
        }
    });

    window.SubscriptionList = Backbone.Collection.extend({
        debuggerTag: 'SubscriptionList',
        model: Node,
        initialize: function(spec) {
            // _.bindAll('sync');
            this.url = spec.url;
        },
        /*sync: function(method, model, success, error) {
            if (model === this) {
                model = _.defaults({url: })
            }
        }*/
        parse: function(response) {
            return _.map(response,
                function(s) { return s.subscription; });
        }
    });

    //$('#loading').dialog({ autoOpen: false });
    window.Node = Backbone.Model.extend({
        debuggerTag: 'Node',
        // id is the -name of the node, e.g. Node[foo@bar] => id = foo
        defaults: {
            id: 'nonode@nohost',
            status: 'unknown',
            info: []
        },
        parse: function(response) {
            return response.node_info;
        }
    });

    window.NodeSet = Backbone.Collection.extend({
        debuggerTag: 'NodeSet',
        url: '/service/nodes',
        model: Node,
        parse: function(response) {
            return _.map(response,
                function(ni) { return ni.node_info; });
        }
    });

    // TODO: use backbone's Model#refresh to do all this.....

    window.Session = Backbone.Model.extend({
        debuggerTag: 'Session',
        defaults: {
            version: '0.0.1',
            host: document.location.host,
            serviceUrl: 'service',
            connected: false,
            sessionId: ''
        },

        websocketUrl: function() {
            return 'ws://' +
                    this.get('host') + '/'
                    + this.get('sessionId');
        },

        login: function() {
            // TODO: make this synchronous or face annoying bugs later on....
            var jqXHR = Service.postForm('/service/login', this.toJSON());
            if (jqXHR.status != 200) {
                this.set({connected: false});
                return false;
            } else {
                this.set({sessionId: $.cookie("sid")});
                console.debug(_.template('connecting to ${ws}',
                                {ws: this.websocketUrl()}));
                //$.cookie("nodewatch.user", this.get('username'));
                var username = this.get('username');
                if (username != undefined) {
                    $.cookie('nodewatch.user', username, {
                        expires: 7,
                        path: '/',
                        domain: document.domain,
                        secure: false
                    });
                }
                this.websocketConnect();
                return true;
            }
        },

        hasCookie: function() {
            return $.cookie('sid') != null;
        },

        websocketConnect: function() {
            var ws = new WebSocket(this.websocketUrl());
            this.set({websocket: ws});
            this.set({connected: true});
            return this;
        },

        toJSON: function() {
            return {
                username: this.get('username'),
                password: this.get('password'),
            };
        }
    });

    window.LoginView = Backbone.View.extend({
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

    window.CollectionView = Backbone.View.extend({
        initialize: function(spec) {
            _(this).bindAll('render');
            this.directives = spec.directives;
            this.collection.bind('change', this.render);
            this.collection.bind('add', this.render);
            this.collection.bind('remove', this.render);
            this.collection.bind("refresh", this.render);
        },
        render: function() {
            this._rendered = true;
            /*$(this.el)
                .empty()
                .directives(this.directives)
                .render(this.collection.toJSON());*/
            Notify.show('Skipping rendering for now.....');
        }
    });

    // TODO: seperate into Application[Model] and ApplicationView

    window.ApplicationView = Backbone.View.extend({
        debuggerTag: 'ApplicationView',
        model: Session,
        templates: {
            subscription: _.template('/service/subscriptions/${username}')
        },
        /*events: {
            "click input.loginButton" : "login"
        },*/
        initialize: function() {
            _.bindAll(this, 'render', 'handleConnected');
            this.model.bind('change:connected', this.handleConnected);
            this.model.view = this;

            var loginPanel = $('#loginPanel');
            this.loginView = new LoginView({el: loginPanel, model: this.model});
            this.el.tabs();

            this.nodes = new NodeSet();
            this.nodeListView = new CollectionView({
                directives: $('#nodes-template'),
                collection: this.nodes
            });
        },
        render: function() {
            var session = arguments.length >= 1 ? arguments[0] : this.model;
            if (session.get('connected') == true) {
                console.debug("Session Already Established - rendering app.");
                this.loginView.hide();
                this.el.show();
                this.refreshSubscriptions();
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
        refreshSubscriptions: function() {
            var username = this.model.get('username');
            /*
            this.subscriptions = new SubscriptionList({
                url: this.templates.subscription({user: username}),
                el: this.$("#subscriptions")
            });
            */
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

    window._session = new Session({username: $.cookie('nodewatch.user')});
    window._application =
        new ApplicationView({el: $('#application'), model: _session});
    _application.render();
});
