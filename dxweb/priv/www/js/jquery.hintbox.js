(function($) {
    $.fn.hintBox = function(text) {
        var context = this;
        return this.each(function() {
            var id = '#' + this.id;
            $(id).focus(function(){
                context.toggleHintBox(id, false, text);
            });
            $(id).blur(function(){
                if ($(id).val().length == 0) {
                    context.toggleHintBox(id, true, text);
                }
            });
            context.toggleHintBox(id, true, text);
        });
    };

    $.fn.toggleHintBox = function(id, on, text) {
        if (on) {
            $(id).addClass('hint');
            $(id).val(text);
        } else {
            if ($(id).hasClass('hint')) {
                $(id).val('');
            }
            $(id).removeClass('hint');
        }
    };
})(jQuery);
