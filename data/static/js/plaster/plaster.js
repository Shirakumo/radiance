


$(function(){
    if(window.mirrorTheme != undefined && window.mirrorTheme != "default")
        $("<link>").appendTo($("head"))
                   .attr({type: "text/css", rel: "stylesheet"})
                   .attr("href", "/static/js/plaster/codemirror-3.21/theme/"+window.mirrorTheme+".css");
        
    function setupEditor(i, editor){
        var mirror;
        
        function setHeight(height){
            $(editor).height(height);
            mirror.setSize(null, height);
        }

        function initializeMirror(){
            var code = $(".code", editor).text();
            mirror = CodeMirror(editor, {
                value: code,
                theme: window.mirrorTheme,
                mode: $(editor).data("mode"),
                lineNumbers: true,
                styleActiveLine: true,
                matchBrackets: true
            });
            $(".code", editor).text("").hide();
            
            var mirrorsize = $(".CodeMirror-sizer", editor).height();
            var editorsize = $(editor).height();
            if($(editor).parent().hasClass("annotation"))
                setHeight(Math.max(Math.min(editorsize, mirrorsize), 50));
            else
                setHeight(Math.max(mirrorsize, 150));
        }

        function initializeResize(){
            $(".editorresize", editor).on("mousedown.editorresize"+i, function(e){
                $(this).addClass("dragging");
                
                $("html").on("mousemove.editorresize"+i, function(e){
                    var offset = e.pageY - $(".editorbar", editor).height()/2;
                    if(offset > $(editor).offset().top){
                        $(".editorbar", editor).offset({top: offset});
                        setHeight($(".editorbar", editor).position().top);
                    }
                });
                
                e.preventDefault();
            });        
            $("html").on("mouseup.editorresize"+i, function(){
                if($(".editorresize", editor).hasClass("dragging")){
                    $(".editorresize", editor).removeClass("dragging");
                    $(this).off("mousemove.editorresize"+i);
                }
            });
        }

        $(editor).submit(function(){
            $(".code", editor).text(mirror.getValue());
        });

        var mode = $(editor).data("mode");
        if(mode != undefined && mode != "raw" && mode != "text"){
            $.getScript("/static/js/plaster/codemirror-3.21/mode/"+mode+"/"+mode+".js")
                .done(function(){ initializeMirror(); })
                .fail(function(){ $(editor).data("mode", null); initializeMirror(); }); 
        }else{
            $(editor).data("mode", null);
            initializeMirror();
        }

        initializeResize();
    }

    $(".editor").each(function(i, editor){
        setupEditor(i, editor);
    });
});
