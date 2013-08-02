$(function (){
	$("#content form").hide();

	$("#panel li").click(function(){
		$("#panel li").removeClass("active");
		$(this).addClass("active");
		$("#content form").hide();
		$("#content #"+$("a", this).text().toLowerCase()).show();
	});

	if($("#panel li.active").length == 0)
		$("#panel li").eq(0).click();
});