


$(function(){
    function setupEditor(i, editor){
        var mirror;
        
        function setHeight(height){
            $(editor).height(height);
            mirror.setSize(null, height);
        }
        
        var code = $(".code", editor).text();
        mirror = CodeMirror(editor, {
            value: code,
            theme: "base16-dark",
            mode: "commonlisp",
            lineNumbers: true,
            styleActiveLine: true,
            matchBrackets: true
        });
        $(".code", editor).remove();

        var mirrorsize = $(".CodeMirror-sizer", editor).height();
        var editorsize = $(editor).height();
        setHeight((mirrorsize<editorsize) ? mirrorsize : editorsize);
        
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
            $(".editorresize", editor).removeClass("dragging");
            $(this).off("mousemove.editorresize"+i);
        });
    }
    
    $(".editor").each(function(i, editor){
        setupEditor(i, editor);
    });
});
