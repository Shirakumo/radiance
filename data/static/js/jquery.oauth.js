
$.fn.oauth = function() {
	var $this = $(this);
	var $lis = $('li', $this);

	$('input[type="radio"]', $this).css("display","none");
	$lis.click(function (){
		$('input[type="radio"]', $this).removeAttr("checked");
		$('.highlight', $this).removeClass("highlight");

		$('input[type="radio"]', $(this)).attr("checked", "checked");
		$(this).addClass("highlight");
	});

	$($lis[0]).click();

	return this;
}