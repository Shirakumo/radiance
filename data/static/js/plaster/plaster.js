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
            //$(".editorbar", editor).css({top: height});
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
                setHeight(Math.min(Math.max(mirrorsize, editorsize),
                                   //Estimate size to fill window
                                   $(window).height() - $(editor).offset().top - $("footer").height()*2 - ($(".editorbar", editor).offset().top - $("#content").height())));
        }
        
        initializeMirror(null);

        function initializeResize(){
            $(".editorresize", editor).on("mousedown.editorresize"+i, function(e){
                $(this).addClass("dragging");
                
                $("html").on("mousemove.editorresize"+i, function(e){
                    var offset = e.pageY - $(".editorbar", editor).height()/2;
                    if(offset > $(editor).offset().top){
                        setHeight(offset-$(editor).offset().top);
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

        mirror.setEditorMode = function(modes, mime, failfunc){
            //Use first class functions to delegate editor mode set until all modes are loaded.
            var funcs = [ function(){mirror.setOption("mode", mime);} ];
            var modes = modes.split(",");
            for(var i=0; i<modes.length; i++){
                (function(mode, prevfunc){
                    funcs.push(function(){
                        console.log("Loading mode "+mode);
                        $.getScript("/static/js/plaster/codemirror-3.21/mode/"+mode+"/"+mode+".js")
                            .done(function(){prevfunc();})
                            .fail(function(){console.log("Failed to load mode "+mode); failfunc();});});
                })(modes[i], funcs[i]);
            }
            funcs[modes.length]();
        }

        $("button[type=\"submit\"]", editor).click(function(){
            $(".code", editor).text(mirror.getValue());
        });

        $(".editorresize", editor).dblclick(function(){
            setHeight($(".CodeMirror-sizer", editor).height()+20);
        });

        var mode = $(editor).data("mode");
        var mime = $(editor).data("mime");
        if(mode == undefined) mode = $("#typeselect option:selected").data("mode");
        if(mime == undefined) mime = $("#typeselect option:selected").val();
        if(mode != undefined && mode != "text" && mode != "default"){
            mirror.setEditorMode(mode, mime, function(){});
        }
        $(editor).data("mode", null);

        initializeResize();
    }

    $(".editor").each(function(i, editor){
        setupEditor(i, editor);
    });

    $("#typeselect").change(function(){
        var mime = this.value;
        var mode = $("option:selected", this).data("mode");
        if(mode != "default"){
            $("#maineditor .editor").data("mirror").setEditorMode(mode, mime, function(){});
        }else{
            $("#maineditor .editor").data("mirror").setOption("mode", "default");
        }
    });

    if($("#viewselect option:selected").val() != "3")
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
        $(".pastelist").each(function(){
            $(this).dataTable({
	        "bPaginate": false,
	        "bFilter": true,
	        "bSort": true,
	        "bInfo": false,
                "aaSorting": [[ $("thead th", this).length-1, "desc"]]
            });
        });
    }
});
