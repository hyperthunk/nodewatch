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

NodeController = Backbone.Controller.extend({
    routes: {
        'nodes/:node': 'showNode',
        'nodes/:node/status': 'showStatus',
        'nodes/:node/sensors/system': 'showSystemStats',
        'nodes/:node/sensors/process': 'showProcessStats',
        'nodes/:node/sensors/network': 'showNetworkStats'
    },
    initialize: function() {
        _.bindAll(this, 'showStatus', 'showSystemStats', 
                        'showNetworkStats', 'showProcessStats');
        this.application = arguments[0];
    },
    showNode: function(node) {
        var nodeDetail = $('#node-detail');
        if (nodeDetail[0].__view != undefined) {
            nodeDetail[0].__view.remove();
        }
        var nodes = this.application.get('nodes');
        var target = nodes.get(node);
        if (target != undefined) {
            // TODO: support removing these objects...
            var primaryView = new NodeDetailView({
                model: target, 
                el: nodeDetail
            });
            
            nodeDetail[0].__view = primaryView;
            
            // NB: prior to rendering the primary content area, none of the 
            // target DOM nodes for subsequent views exist
            primaryView.render();
            
            var nodeStatusFrame = nodeDetail.find('div.status-frame');
            var statusView = new NodeStatusView({
                model: target,
                el: nodeStatusFrame
            });
            statusView.render();
            
            var systemTraceFrame = nodeDetail.find('div.system-frame');
            /*var systemStatusMessages = 
                new SystemStats([], {node: target.get('id')});*/
            var systemView = new SystemStatsView({
                node: target.get('id'),
                el: systemTraceFrame
            });
            systemTraceFrame[0].__view = systemView;
        }
    },
    showSystemStats: function(node) {
        // TODO: ensure the content has loaded
        var nodeDetail = $('#node-detail');
        if (nodeDetail[0].__view != undefined) {
            if (nodeDetail[0].__view.model.get('id') != node) {
                showNode(node);
            }
        }
        $('#node-detail div.ui-widget').hide();
        $('#node-detail div.system-frame').show();
    },
    showStatus: function(node) {
        // TODO: ensure the content has loaded
        var nodeDetail = $('#node-detail');
        if (nodeDetail[0].__view != undefined) {
            if (nodeDetail[0].__view.model.get('id') != node) {
                showNode(node);
            }
        }
        $('#node-detail div.ui-widget').hide();
        $('#node-detail div.status-frame').show();
    }
});
