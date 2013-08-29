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
	var checkboxes = $('input[type="checkbox"]', $(this).parent().parent().parent().parent());
	checkboxes.prop("checked", $(this).is(":checked"));
    });

    function submitTableExtension(table){

    };

    $(".tablebox table.extendable").each(function(){
        var $this = $(this);
        var $insertRow = jQuery("<tr />", {class: "tableExtensionRow"});
        $("thead tr th", $this).each(function(){
            var $td = jQuery("<td />");
            var type = $(this).attr("data-field-type");
            if (type == "null"){
            }else if (type == "actions"){
                var $input = jQuery("<input />", {
                    type: "submit",
                    value: "Add"
                });
                $input.click(function(){submitTableExtension($this);});
            }else{
                var $input = jQuery("<input />", {
                    name: $(this).attr("data-field-name"),
                    type: type,
                    width: $(this).attr("data-field-width")
                });
            }
            $td.append($input);
            $insertRow.append($td);
        });
        $("tbody", $this).append($insertRow);
    });

    $(".tablebox table").dataTable({
	"bPaginate": false,
	"bFilter": true,
	"bSort": true,
	"bInfo": false,
    });

    // Complex UI

    function AppViewModel() {

    }

    ko.applyBindings(new AppViewModel());
});
