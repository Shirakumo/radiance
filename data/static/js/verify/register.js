$(function() {
    function qs(key) {
        key = key.replace(/[*+?^$.\[\]{}()|\\\/]/g, "\\$&"); // escape RegEx meta chars
        var match = location.search.match(new RegExp("[?&]"+key+"=([^&]+)(&|$)"));
        return match && decodeURIComponent(match[1].replace(/\+/g, " "));
    }


    // Basic Panel UI
    $("#panel>ul>li>ul").hide();

    $("#panel>ul>li>a").click(function(){
        $("ul", $(this).parent()).stop(true).slideToggle();
    });

    $("#panel>ul>li>ul>li>a, #panel>ul>li>a").click(function(){
        $("#panel .active").removeClass("active");
        $(this).parent().addClass("active");
        $(this).parent().parent().parent().addClass("active");
    });

    $("#content form").hide();

    $("#panel li").click(function (){
        $form = $("#content #"+$("a", $(this)).text().toLowerCase());
        if($form.length > 0){
            $("#content form").hide();
            $form.show();
        }
    });

    if(qs("panel"))
        $("#panel li."+qs("panel")+" a").click();

    if($("#panel .active").length == 0)
        $("#panel>ul>li>ul>li>a").eq(0).click();

    $("#panel>ul>li>ul>li.active").parent().show();


});