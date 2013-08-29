function pad(string, n, padding){
    if(n == undefined) n = 2;
    if(padding == undefined) padding = " ";
    var truen = n-(string+"").length;
    return new Array(truen+1).join(padding) + string;
}

function getDateAsDateString(date){
    if(date == undefined) date = new Date();
    var d = date.getDate();
    var m = date.getMonth() + 1;
    var y = date.getFullYear();
    return '' + y + '.' + pad(m, 2, "0") + '.' + pad(d, 2, "0");
}

function getDateAsDateTimeString(date){
    if(date == undefined) date = new Date();
    var h = date.getHours();
    var m = date.getMinutes();
    var s = date.getSeconds();
    return getDateAsDateString(date) + ' ' + pad(h, 2, "0") + ':' + pad(m, 2, "0") + ':' + pad(s, 2, "0");
}


$(function(){
    $(".unixtime").each(function(){
        $this = $(this);
        $this.text(getDateAsDateTimeString(new Date(parseInt($this.text())*1000)));
    });

    $("header#topmenu li").hover(function(){
        $(this).children("ul").stop(true, true).fadeIn(200);
    }, function (){
        $(this).children("ul").stop(true, true).fadeOut(200);
    });
});
