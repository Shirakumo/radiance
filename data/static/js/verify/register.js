$(function() {
function AppViewModel() {
	var self = this;
    self.username = ko.observable($("#username").val());
    self.displayname = ko.observable($("#displayname").val());
    self.pw1 = ko.observable($("#password-in-1").val());
    self.pw2 = ko.observable($("#password-in-2").val());
    self.usernameok = ko.observable("");
    self.passwordmatch = ko.observable("");
    self.loginsok = ko.observable("");
    self.loginsokexpl = ko.observable("");
    self.registerbutton = ko.observable(false);

    self.computeLogins = function (){
    	if ($("#mechanisms .linked").length > 0){
    		self.loginsokexpl("");
    		self.loginsok("");
    	}else{
    		self.loginsokexpl(" Please choose at least one login.");
    		self.loginsok("icon-remove-sign");
    	}
    };

    self.computeAllOk = function (){
    	if(self.usernameok() == "icon-ok-sign" &&
    	   self.passwordmatch() == "icon-ok-sign" &&
    	   self.loginsok() == ""){
    		self.registerbutton(true);
    	}else{
    		self.registerbutton(false);
    	}
    };

    self.passwordmatchexpl = ko.computed(function (){
	    $("#password").removeClass("linked");
	    var returnval = "", returnexpl = "";
    	if (self.pw1().length == 0){
    	}else if ((self.pw1() == self.pw2())){
    		if(self.pw1().length >= 8){
	    		$("#password").addClass("linked");
	    		returnval = "icon-ok-sign";
	    	}else{
	    		returnexpl = " Password too short (min. 8)";
	    		returnval = "icon-remove-sign";
	    	}
    	}else{
    		returnexpl = " Passwords do not match!";
    		returnval = "icon-remove-sign";
    	}
    	self.passwordmatch(returnval);
    	self.computeLogins();
    	self.computeAllOk();
    	return returnexpl;
    });

    self.usernameokexpl = ko.computed(function (){
    	var returnval = "", returnexpl = "";
    	if (self.username().length == 0){
    		returnexpl = " Username is required!";
    		returnval = "icon-remove-sign";
    	}else if (self.username().match(/^[a-z0-9]+$/i)){
    		returnval = "icon-ok-sign";
    	}else{
    		returnexpl = " Has to be Alphanumeric (0-9 a-Z)";
    		returnval = "icon-remove-sign";
    	}
    	self.usernameok(returnval);
    	self.computeAllOk();
    	return returnexpl;
    });
}

// Activates knockout.js
ko.applyBindings(new AppViewModel());
});