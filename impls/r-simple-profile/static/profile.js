$(function(){
    var customColor = $("#profile-custom #color").text();
    var customBackground = $("#profile-custom #background").text();

    if(customColor !== ""){
        $("*").each(function(){
            if($(this).css("border-color").indexOf("rgb(0, 136, 238)") != -1){
                $(this).css("border-color", customColor);
            }
            if($(this).css("background-color") === "rgb(0, 136, 238)"){
                $(this).css("background-color", customColor);
            }
        });
    }

    if(customBackground !== ""){
        // custom bg processing
        var bgprops = {"background-size": ["cover", "contain"],
                       "background-attachment": ["scroll", "fixed", "local"]};
        var cssToSet = {};
        for(var prop in bgprops){
            var vals = bgprops[prop];
            for(var i = 0; i < vals.length; i++){
                var val = vals[i];
                if(customBackground.indexOf(val) != -1){
                    customBackground = customBackground.replace(val, "");
                    cssToSet[prop]=val;
                }
            }
        }
        // We need to do it in this very order
        $("html").css("background", customBackground);
        $("html").css(cssToSet);
    }
});
