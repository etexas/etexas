//Initialize ajax monitoring code
if (typeof window.sqaAjax === "undefined") {
	window.sqaAjax = {
		total: 0//total of all requests awaiting response
	};
    //Takes a regex and returns the total of all open requests
    //with endpoints matching the regex and case-insensitive method (e.g. "PUT", "get")
	window.sqaAjax.matchedOpen = function (pattern, method) {
		var keys = [];
		var openTotal = 0;
        var meth = method.toLowerCase();
		for (var key in this)
			if (this.hasOwnProperty(key) && key.match(/^[a-z]+\s\/rest/)) {
				keys.push(key);
			}
		var filt = keys.filter(function (k) {
				var arr = k.split(' ');
                console.log(arr[0]);
                console.log(arr[1]);
                return arr[0] == meth && arr[1].match(pattern) !== null;
			});
		if (filt === null || filt.length < 1) {
			return -1;
		}
		filt.forEach(function (f) {
			var temp = window.sqaAjax[String(f)];
			if (typeof temp === 'number') {
				openTotal += temp;
			}
		});
		return openTotal;
	};
    //Store the original implementations
	var open = window.XMLHttpRequest.prototype.open;
	var send = window.XMLHttpRequest.prototype.send;
    
    //Wraps open, storing the method and url endpoint of the request
	function openReplacement(method, url, async, user, password) {
		this._url = url;
        this._method = method.toLowerCase();
        this._method_and_url = this._method + " " + this._url;
		return open.apply(this, arguments);
	};
    //Wraps send, incrementing the total and method+url-specific total of open requests
	function sendReplacement(data) {
		if (this.onreadystatechange) {
			this._onreadystatechange = this.onreadystatechange;//store original function
		}
		if (this._method_and_url) {
			var val = window.sqaAjax[this._method_and_url];//store method and url as a key
			if (typeof val !== "number") {
				val = 0;
			}
			window.sqaAjax[this._method_and_url] = ++val;//increment method+url-specific total
			window.sqaAjax.total++;//increment total
		}
		this.onreadystatechange = onReadyStateChangeReplacement;//overwrite with wrapped function
		return send.apply(this, arguments);
	};
    //Wraps onreadystatechange function, decrementing total and method+url-specific total of open requests
    //when the ajax call completes
	function onReadyStateChangeReplacement() {
		if (this.readyState == 4 && this._method_and_url) {
			window.sqaAjax[this._method_and_url]--;//decrement method+url-specific total
			window.sqaAjax.total--;//decrement total
		}
		if (this._onreadystatechange) {
			return this._onreadystatechange.apply(this, arguments);
		}
	};
    //Replace open and send with wrapper functions
	window.XMLHttpRequest.prototype.open = openReplacement;
	window.XMLHttpRequest.prototype.send = sendReplacement;
};