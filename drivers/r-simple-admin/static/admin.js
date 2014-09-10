$(function(){
    // Basic Panel UI
    //$("#panel>ul>li>ul").hide();

    $("#panel>ul>li>a").click(function(){
	$("ul", $(this).parent()).stop(true).slideToggle();
    });

    $("#panel>ul>li>ul>li").click(function(){
	$("#panel .active").removeClass("active");
	$(this).addClass("active");
	$(this).parent().parent().addClass("active");
    });

    $("#panel>ul>li>ul>li.active").click().parent().show();

    // Table checkboxes
    $(".select-all").click(function(){
	var checkboxes = $('td:first-child input[type="checkbox"]', $(this).parent().parent().parent().parent());
	checkboxes.prop("checked", $(this).is(":checked"));
    });

    function submitTableExtension(table){

    };

    $(".tablebox table.extendable").each(function(){
        var $this = $(this);
        
    });

    $(".tablebox table.interactive").dataTable({
	"bPaginate": false,
	"bFilter": true,
	"bSort": true,
	"bInfo": false,
    });
});
