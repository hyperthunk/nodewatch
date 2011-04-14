//
// Erlang System Monitoring Dashboard: Helpers
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

// provides *growl* notifications on screen.
Notify = {
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

NoOp = function() {};

// *static* façade for service calls
Service = {
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
