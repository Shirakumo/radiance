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

        function initializeMirror(mode){
            var code = $(".code", editor).text();
            mirror = CodeMirror(editor, {
                value: code,
                theme: window.mirrorTheme,
                mode: mode,
                lineNumbers: true,
                lineWrapping: true,
                styleActiveLine: true,
                matchBrackets: true
            });
            $(editor).data("mirror", mirror);
            $(".code", editor).text("").hide();
            
            var mirrorsize = $(".CodeMirror-sizer", editor).height()+20;
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

            $(".editorwrapping", editor).click(function(){
                mirror.setOption("lineWrapping", !mirror.getOption("lineWrapping"));
            });
        }

        $(editor).submit(function(){
            $(".code", editor).text(mirror.getValue());
        });

        var mode = $(editor).data("mode");
        if(mode == undefined) mode = $("#typeselect")[0].value;
        if(mode != undefined && mode != "raw" && mode != "text"){
            $.getScript("/static/js/plaster/codemirror-3.21/mode/"+mode+"/"+mode+".js")
                .done(function(){ initializeMirror(mode); })
                .fail(function(){ initializeMirror(null); }); 
        }else{
            $(editor).data("mode", null);
            initializeMirror();
        }

        initializeResize();
    }

    $(".editor").each(function(i, editor){
        setupEditor(i, editor);
    });

    $("#typeselect").change(function(){
        var mode = this.value;
        if(mode != "raw" && mode != "text"){
            $.getScript("/static/js/plaster/codemirror-3.21/mode/"+mode+"/"+mode+".js")
                .done(function(){
                    $("#maineditor .editor").data("mirror").setOption("mode", mode);
                });
        }else{
            $("#maineditor .editor").data("mirror").setOption("mode", "default");
        }
    });

    $("#viewpassword").hide();
    $("#viewselect").change(function(){
        if(this.value == "3")
            $("#viewpassword").show();
        else
            $("#viewpassword").hide();
    });

    $("#maineditor .editorbar .paste").click(function(){
        $("#maineditor .code").text($("#maineditor .editor").data("mirror").getValue());
    });

    if($(".pastelist").length > 0){
        $(".pastelist").dataTable({
	    "bPaginate": false,
	    "bFilter": true,
	    "bSort": true,
	    "bInfo": false,
        });
    }
});
