function getNowAsDateString(){
    var date = new Date();
    var d = date.getDate();
    var m = date.getMonth() + 1;
    var y = date.getFullYear();
    return '' + y + '.' + (m<=9 ? '0' + m : m) + '.' + (d <= 9 ? '0' + d : d);
}

var primaryColor = "rgb(0, 136, 238)";
var targetColor = "rgb(0, 136, 238)";

function customizeProfile(color, background){
    function change_color($this, style){
        if($this.css(style) == primaryColor){
            $this.css(style, targetColor);
        }
    }

    if(background && background != "null"){
        $("#profile").css("background", background);
    }
    
    if(color && color != "null"){
        targetColor = color;
        $("*").each(function(){
            $this = $(this);
            change_color($this, "border-left-color");
            change_color($this, "border-right-color");
            change_color($this, "border-top-color");
            change_color($this, "border-bottom-color");
            change_color($this, "background-color");
            change_color($this, "color");
        });
        primaryColor = targetColor;
    }
}

$(function(){
    $("#profile-comments-submit textarea").css("min-height", "10px").click(function(){
        if($(this).height()<100)
            $(this).css("height", "100px");
    });

    /*$("#profile-comments-submit input[type=\"submit\"]").click(function(){
        $form = $("#profile-comments-submit");
        $textarea = $("#profile-comments-submit textarea");
        $clone = $("#profile-comments ul li").first().clone();
        $("header img", $clone).attr("src", $("header img", $form).attr("src"));
        $("header a", $clone).text($("header a", $form).text());
        $(".date", $clone).text(getNowAsDateString());
        $(".text", $clone).html("<p>" + $textarea.val() + "</p>");
        $clone.prependTo("#profile-comments ul").hide().fadeIn();
        $("#profile-comments-submit textarea").val("");
        return false;
    });*/
});
