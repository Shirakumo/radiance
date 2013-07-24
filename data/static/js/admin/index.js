$(function(){
	$("#panel>ul>li>ul").hide();

	function AppViewModel() {

	}

	ko.applyBindings(new AppViewModel());

	$("#panel>ul>li>a").click(function(){
		$("ul", $(this).parent()).stop(true).slideToggle();
	});

	$("#panel>ul>li>ul>li").click(function(){
		$("#panel .active").removeClass("active");
		$(this).addClass("active");
		$(this).parent().parent().addClass("active");
	});

	$("#panel>ul>li>ul>li.active").click().parent().show();
});