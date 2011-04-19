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
                'a@class+': function(arg) {
                    if (arg.item.status == 'nodedown') {
                        return 'ui-state-error';
                    } else {
                        return '';
                    }
                },
                'a strong': 'el.id',
                'a span@class': function(arg) {
                    if (arg.item.status == 'nodeup') {
                        return '';
                    } else {
                        return 'ui-icon ui-icon-alert';
                    }
                }
            }
        }
    },
    // NB: this is highly inefficient - we should render individual models...
    initialize: function() {
        _(this).bindAll('nodeup', 'nodedown', 'doWithNode');
        CollectionView.prototype.initialize.call(this, arguments);
        _application.bind('event:nodeup', this.nodeup);
        _application.bind('event:nodedown', this.nodedown);
    },
    nodeup: function(eventData) {
        var node = this.collection.get(eventData.node_info.id);
        if (node === undefined) {
            node = eventData.node_info;
            this.collection.add([node]);
        }
        // TODO: verify the above and remove this check
        if (node != undefined) {
            var elem = $("dd a[href*='" + node.id + "']", this.el);
            elem.removeClass('ui-state-error')
                .addClass('node-state-ok')
                .find('span')
                .removeClass('ui-icon')
                .removeClass('ui-icon-alert');
        }
    },
    // TODO: de-duplicate these two functions.
    nodedown: function(eventData) {
        var node = this.collection.get(eventData.node_info.id);
        if (node === undefined) {
            node = eventData.node_info;
            this.collection.add([node]);
        }
        // TODO: verify the above and remove this check
        if (node != undefined) {
            var elem = $("dd a[href*='" + node.id + "']", this.el);
            elem.addClass('ui-state-error')
                .removeClass('node-state-ok')
                .find('span')
                .addClass('ui-icon')
                .addClass('ui-icon-alert');
        }
    }
});

NodeStatusView = Backbone.View.extend({
    debuggerTag: "NodeStatusView",
    model: Node,
    directives: {
        'dd.node-name': 'id',
        'span.status-text': 'status',
        'span.ui-icon@class+': function(ctx) {
            if (ctx.context.status == 'nodeup') {
                return ' icon-power-on';
            } else {
                return ' icon-power-off';
            }
        },
        'dd.node-uptime': function(ctx) {
            return Math.round(ctx.context.uptime.elapsed / 60);
        },
        'dd.node-downtime': function(ctx) {
            return Math.round(ctx.context.downtime.elapsed / 60);
        },
    },
    template: undefined,
    initialize: function() {
        _.bindAll(this, 'render', 'remove', 'hide');
        this.compileTemplate();
    },
    render: function() {
        this.el.removeClass('ui-helper-hidden')
               .addClass('ui-frame-active')
               .html(this.template(this.model.toJSON()))
               .show();
        return this;
    },
    hide: function() {
        this.el.removeClass('ui-frame-active')
               .addClass('ui-helper-hidden');
    },
    remove: function() {
        this.el.empty();
    },
    compileTemplate: function() {
        this.template = this.el.compile(this.directives);
    }
});

SystemStatsView = Backbone.View.extend({
    debuggerTag: "SystemStatsView",
    initialize: function(opts) {
        _.bindAll(this, 'handleSysEvent');
        this.node = opts.node;
        var eventKey = 'event:system:' + this.node;
        console.debug('subscribing to ' + eventKey);
        this.template = $(this.el)
            .clone().removeClass('ui-helper-hidden')
            .compile({
            'dd.now': 'now',
            'dd.atom': 'atom',
            'dd.atom_used': 'atom_used',
            'dd.binary': 'binary',
            'dd.code': 'code',
            'dd.context_switches': 'context_switches',
            'dd.cores': 'cores',
            'dd.system': 'system',
            'dd.total': 'total',
            'dd.total_ram': 'total_ram',
            'dd.ets': 'ets',
            'dd.gc_reclaimed': 'gc_reclaimed',
            'dd.gcs': 'gcs',
            'dd.processes': 'processes',
            'dd.processes_used': 'processes_used',
            'dd.procs': 'procs',
            'dd.reductions': 'reductions',
            'dd.run_queue': 'run_queue'
        });
        _application.bind(eventKey, this.handleSysEvent);
    },
    handleSysEvent: function(ev) {
        console.debug("rendering " + ev.now);
        // console.debug(ev);
        this.el.html(this.template(ev));
    },
    remove: function() {
        // this.collection.destroy();
        this.el.empty();
    }
});

NodeDetailView = Backbone.View.extend({
    debuggerTag: 'NodeDetailView',
    model: Node,
    events: {
        'click a': 'navbarItemClicked'
    },
    initialize: function() {
        _.bindAll(this, 'render', 'remove', 'navbarItemClicked');
    },
    navbarItemClicked: function(ev) {
        // TODO: move the item with demo-config-on to here
        var elem = this.$('.demo-config-on').first();
        if (elem.size() > 0) {
            elem.removeClass('demo-config-on');
        }
        // ev points to the anchor that was clicked,
        // but we are going to style the outer <li/>
        $(ev.currentTarget).parent().addClass('demo-config-on');

        // we need to clear the current (active) marker
        // *but* the next view will re-add the marker itself
        this.$('.ui-frame-active')
            .first()
            .removeClass('ui-frame-active')
            .hide();
    },
    render: function() {

        // TODO: this is horribly wasteful - we should cache the view somehow....

        this.el = $(this.el);
        var domId = this.el.attr('id');
        var templateClass = '.' + domId + '-template';
        this.el.empty()
               .html($(templateClass).html())
               .removeClass(templateClass)
               .attr('id', domId);
        var nodeId = this.model.get('id');
        this.el.find("a[href*='#nodes/:node']").each(function() {
            var el = $(this);
            el.attr('href', el.attr('href').replace(':node', nodeId));
        });
        // this.el.html(render({elements: this.model.toJSON()}));
        return this;
    },
    remove: function() {
        // TODO: find a better place for this to live....
        this.$('demo-frame div').each(function() {
            if (this.__view != undefined) {
                console.debug("removing sub-view");
                this.__view.remove();
            }
        });
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
    events: {
        'click .subscription-button' : 'toggleSubscriptions'
    },
    initialize: function() {
        _.bindAll(this, 'render', 'handleConnected', 'refreshData',
                        'toggleSubscriptions', 'subscriptionStatusChanged');
        this.model.get('session').bind('change:connected', this.handleConnected);
        this.model.bind('change:subscriptionStatus', this.subscriptionStatusChanged);
        this.model.get('session').bind('websock:data', function(ev) {
            console.debug('websocket data:');
            console.debug(ev);
        });

        var loginPanel = $('#loginPanel');
        this.loginView = new LoginView({el: loginPanel, model: this.model.get('session')});
        this.el.tabs();

        this.nodeListView = new NodeListView({
            el: $('#navbar-node'),
            collection: this.model.get('nodes')
        });

        $('#subscriptions button').button({
            icons: { primary: "ui-icon-power" }
        });
    },
    toggleSubscriptions: function(ev) {
        var button = $(ev.currentTarget);
        if (button.hasClass('ui-state-error')) {
            this.model.activateSubscriptions();
        } else {
            this.model.deactivateSubscriptions();
        }
    },
    subscriptionStatusChanged: function(_, status) {
        var button = this.$('.subscription-button');
        var txt = button.text();
        if (button.hasClass('ui-state-error')) {
            button.removeClass('ui-state-error');
        } else {
            button.addClass('ui-state-error');
        }
    },
    render: function() {
        var session = arguments.length >= 1 ? arguments[0] : this.model.get('session');
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
        this.model.get('nodes').fetch();
        var subscriptions = this.model.get('subscriptions');
        subscriptions.url = '/service/subscriptions/' + username;

        // TODO: this is completely screwed - get the controller mechanism working properly...

        var nodes = new NodeController(_application);
        Backbone.history.start();
    },
    handleConnected: function(_, connected) {
        if (connected) {
            this.render();
        } else {
            Notify.grumble('WebSocket connection broken!\n' +
                           'Attempting to re-establish session....');
            $.cookie("sid", null);
            // this.el.hide();
            if (!this.loginView.render().login()) {
                Notify.show("Unable to establish session!");
            }
        }
        return this;
    }
});

