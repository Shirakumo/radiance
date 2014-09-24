$(function(){
    function setNav(nav, target){
        $("li", nav).click(function(){
            $("nav li").removeClass("active");
            $("#content>article>ul>li").hide();
            $(this).addClass("active");
            console.log($("[data-id="+$(this).data("id")+"]", target));
            $("[data-id="+$(this).data("id")+"]", target).show();
        });
    }

    setNav($("nav .stack"), $("article>.stack"));
    setNav($("nav .restarts"), $("article>.restarts"));
    setNav($("nav .objects"), $("article>.objects"));

    $(".stack .slots").hide();

    $(".stack .arg").click(function(){
        $(".slots", this).slideToggle();
    });

    HighlightLisp.highlight_auto();
});
