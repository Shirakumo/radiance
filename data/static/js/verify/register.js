function AppViewModel() {
    this.username = ko.observable("");
    this.displayname = ko.observable("");
    this.pw1 = ko.observable("");
    this.pw2 = ko.observable("");

    this.passwordmatch = ko.computed(function (){
    	return ((this.pw1() == this.pw2()) && this.pw1().length >= 6)? "O" : "X";
    }, this);
}

// Activates knockout.js
ko.applyBindings(new AppViewModel());