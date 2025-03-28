/*
;;
;; EXCEPT WHERE OTHERWISE NOTED BELOW:
;;
;; Copyright 2002-2011 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 
*/

var doublequote = '\"';

// function createRequest() {
//   var request
// 
//  try {
//    request = new XMLHttpRequest();
//  } catch (trymicrosoft) {
//    try {
//      request = new ActiveXObject('Msxml2.XMLHTTP');
//    } catch (othermicrosoft) {
//      try {
//        request = new ActiveXObject('Microsoft.XMLHTTP');
//      } catch (failed) {
//        request = false;
//      }
//    }
//  }
//
//  if (!request)
//    alert('failed to create XMLHttpRequest');
//
//  return request
//}



function createRequest() {
  return {
    // Mock methods to mimic XMLHttpRequest behavior
    open: function(method, url, async) {
      this.method = method;
      this.url = url;
      this.async = async;
    },
    setRequestHeader: function(header, value) {
      if (!this.headers) this.headers = {};
      this.headers[header] = value;
    },
    send: function(data) {
      var that = this;
      fetch(this.url, {
        method: this.method,
        headers: this.headers,
        body: data
      }).then(response => {
        that.status = response.status;
        that.readyState = 4;
        that.responseText = response.text(); // Note: This returns a Promise, which isn't ideal for direct use here
        that.responseXML = new DOMParser().parseFromString(response.text(), "text/xml");
        that.onreadystatechange(); // Trigger the callback
      }).catch(error => {
        console.error('Fetch operation failed:', error);
        // Here you might want to set some error state or throw an error
        that.readyState = 4;
        that.status = 0; // or some error status
        that.onreadystatechange(); // Trigger the callback to handle error
      });
    },
    readyState: 0,
    status: 0,
    onreadystatechange: function() {},
    responseText: '',
    responseXML: null
  };
}



/**
* returns the absolute left location of an element.
* param: e: element
* return: an integer representing the offset from left.
*/
function getElementLeftPosition(e){
var x=0;
while(e){
x+=e.offsetLeft;
e=e.offsetParent;
}
return x;
}

/**
* returns the absolute top location of an element.
* param: e: element
* return: an integer representing the offset from top.
*/
function getElementTopPosition(e){
var y=0;
while(e){
y+=e.offsetTop;
e=e.offsetParent;
}
return y;
}


function gdlAjax1 (params, asynch)
{
    gdlAjax(null, params, async);
}

function gdlAjax (evt, params, asynch)
{


 if (evt)
 {
  var target;

  if (evt.target) target = evt.target;
  if (evt.srcElement) target = evt.srcElement;

  while ((target.tagName != 'DIV') && (target.tagName != 'BODY')){target = target.parentNode;}

  var x = evt.clientX-getElementLeftPosition(target);
  var y = evt.clientY-getElementTopPosition(target);
 }

  var request = createRequest();
  var url = "/gdlAjax";

  params = params + '&x=' + x + '&y=' + y;

  request.onreadystatechange = function () {gdlUpdate(request)};

  request.open('POST', url, asynch);
  request.setRequestHeader('content-type', 'application/x-www-form-urlencoded');

  request.send(params);

  }


function gdlUpdate (request) {

 if (request.readyState == 1)
   if (document.getElementById('gdlStatus'))
    document.getElementById('gdlStatus').innerHTML = 'Working...';

 if (request.readyState == 2)
   if (document.getElementById('gdlStatus'))
    document.getElementById('gdlStatus').innerHTML = 'Got Error!';

 if (request.readyState == 3)
   if (document.getElementById('gdlStatus'))
     document.getElementById('gdlStatus').innerHTML = 'Almost There...';

 if ((request.readyState == 4) && (request.status == 200))
    {

	var root = request.responseXML.documentElement;
	var children = root.childNodes;
	var myelem;
	var codes;

	for (i=0; i< children.length; i++)
	{
	    var child=children[i];
	    var myid = null;
	    if (child.getElementsByTagName('replaceId')[0].firstChild != null)
            {
		myid = child.getElementsByTagName('replaceId')[0].firstChild.data
            }

	    var newHTML = null;
	    
	    if (child.getElementsByTagName('newHTML')[0].firstChild != null)
            {newHTML = child.getElementsByTagName('newHTML')[0].firstChild.nodeValue}

	    var jsToEval = null;

	    if (child.getElementsByTagName('jsToEval')[0].firstChild != null)
	    {jsToEval = child.getElementsByTagName('jsToEval')[0].firstChild.nodeValue}

	    if (myid && (newHTML != null))
            {
		var myelem = document.getElementById(myid);

		myelem.innerHTML = newHTML;

		if (jsToEval && (jsToEval == 'parseme'))
		{
		    codes = myelem.getElementsByTagName("script");
		    
		    for (var j=0;j<codes.length;j++)
		    {
			var text = codes[j].text;
			if (text) eval(text);
		    }}
		
            }
	    
	    if (jsToEval && (jsToEval != 'parseme') && (jsToEval != ''))
	    eval(jsToEval);
	}


	if (document.getElementById('gdlStatus'))
	{
	    document.getElementById('gdlStatus').innerHTML = 'Done.';
	}
    }}


function b64EncodeUnicode(str) {
      // first we use encodeURIComponent to get percent-encoded UTF-8,
      // then we convert the percent encodings into raw bytes which
      // can be fed into btoa.
      return btoa(encodeURIComponent(str).replace(/%([0-9A-F]{2})/g,
          function toSolidBytes(match, p1) {
              return String.fromCharCode('0x' + p1);
      }));
  }

function encode64(input) {
    return b64EncodeUnicode(input).replace(/\=/g, '');
}



//
// debouncing technique from https://css-tricks.com/snippets/jquery/done-resizing-event/.
//
// More general debouncing function here: https://davidwalsh.name/javascript-debounce-function
//
var resizeTimer;

function gdlResize(rp = 'nil')
{

    // if  (document.getElementById('x3dom-1'))
    // {}
    //else
    //{

    console.log('Starting gdlResize');
    
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function () {
    
	gdlAjax(null, 'args=' + encode64('(:|iid| '+ doublequote + gdliid + doublequote + ' :|bashee| (:%rp% '+ rp + ') :|function| :set-slot! :|arguments| (:viewport-dimensions (:width ' + (document.getElementById('viewport').getBoundingClientRect().width) +  ' :length ' + (document.getElementById('viewport').getBoundingClientRect().height) + ')))'), true );}, 250);

// }

}



function collectMenuSelections(select)
{
    var items = "";
    for (var i = 0; i < select.options.length; i++)
        if (select.options[i].selected)
            items = items + ':|' + select.name + '| ' + doublequote + encode64(select.options[i].value) + doublequote + ' ';

    if (items)
	return(items);
    else
	return(':|' + select.name | '|' + doublequote + encode64('nil') + doublequote + ' ');
}


function loadScript(url){

    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src = url;
    document.getElementsByTagName("head")[0].appendChild(script);
}



