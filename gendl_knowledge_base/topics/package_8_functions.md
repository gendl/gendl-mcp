# Gendl Documentation - package_8_functions

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/7/index.html
Type: reference

```
Function: GWL:CRAWL <-Back Function: Gwl:Crawl CRAWL void Instantiates and ``Crawls'' a given object instance and creates static HTML pages reflecting the instance tree. This means it will recursively follow all the links for the object. By default the files are written into "/tmp/sites/". arguments: part String Names a package-qualified part which should mix in base-html-sheet keyword arguments: host String , Default Value: "localhost" Host on which the server is running port Integer , Default Value: 9000 Port on which the server is running output-root String or pathname , Default Value: "/tmp/sites/[non-package-qualified-part-name]/" Directory where filfes will be written make-part-args Plist , Default Value: NIL Other make-instance arguments to use to initialize the object example: (gwl:crawl "yadd:assembly") <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/14/index.html
Type: reference

```
Function: GWL::SESSION-REPORT <-Back Function: Gwl::Session-Report SESSION-REPORT returns list of instances in a runtime environment Those that are of type session-control-mixin, it provides more detailed information, that can be useful in tracking the session life. Currently, this is intended to run from the lisp command prompt. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/0/index.html
Type: reference

```
Function: GWL:BASE64-DECODE-LIST <-Back Function: Gwl:Base64-Decode-List BASE64-DECODE-LIST list Decodes a base64 string into a Lisp list. arguments: string string <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/16/index.html
Type: reference

```
Macro: GWL:WITH-CL-WHO-STRING <-Back Macro: Gwl:With-Cl-Who-String WITH-CL-WHO-STRING form Sets up body to be evaluated with our with-cl-who return the resulting string instead of side-effecting anything at all to the default *stream*. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/10/index.html
Type: reference

```
Function: GWL:PUBLISH-SHARED <-Back Function: Gwl:Publish-Shared PUBLISH-SHARED void Used to publish a site which is to have a shared toplevel instance tree, and no URI rewriting (i.e. no "/sessions/XXX/" at the beginning of the path). So, this site will appear to be a normal non-dynamic site even though the pages are being generated dynamically. arguments: object-type Symbol The type of the toplevel object to be instantiated keyword arguments: path String , Default Value: "/" Must start with / The URI path to be published , Default Value: NIL Defaults to / NIL , Default Value: NIL paths If you want to give multiple paths , Default Value: (ENSURE-LIST PATH) Overrides path server Allegroserve server object , Default Value: *HTTP-SERVER* If you have additional servers other than the default *http-server* (e.g. an SSL server) then you may want to call this function for each server host Hostname for URI , Default Value: NIL Must be a valid hostname.
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/10/index.html
Type: reference

```
*http-server* (e.g. an SSL server) then you may want to call this function for each server host Hostname for URI , Default Value: NIL Must be a valid hostname. By default comes from the object instance hosts Hostnames for URI , Default Value: NIL Must be valid hostnames. Overrides host key Key for the *instance-hash-table* , Default Value: NIL Defaults to someting reasonable. object if you want to provide an already-instantiated one , Default Value: NIL Can be a list. Will override object-type content-type by default comes from the object instance , Default Value: NIL aserve-args Any other AllegroServe arguments you'd like to provide, defaults to nil , Default Value: NIL object-args Any other object arguments you'd like to provide on instantiation, defaults to nil , Default Value: NIL example: (publish-shared 'site:assembly :host (list "www.genworks.com" "ww2.genworks.com" "mccarthy.genworks.com")) <-Back Copyright © 2025 Genworks ® International . All rights reserved.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/10/index.html
Type: reference

```
ple: (publish-shared 'site:assembly :host (list "www.genworks.com" "ww2.genworks.com" "mccarthy.genworks.com")) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/8/index.html
Type: reference

```
Function: GWL:GWL-MAKE-OBJECT <-Back Function: Gwl:Gwl-Make-Object GWL-MAKE-OBJECT void Used within the context of the body of a :function argument to Allegroserve's publish function, makes an instance of the specified part and responds to the request with a redirect to a URI representing the instance. arguments: req Allegroserve request object, as used in the function of a publish ent Allegroserve entity object, as used in the function of a publish package-and-part String Should name the colon- (or double-colon)-separated package-qualified object name keyword arguments: make-object-args Plist of keys and values , Default Value: NIL These are passed to the object upon instantiation share? Boolean , Default Value: NIL If non-nil, the instance ID will be the constant string ``share'' rather than a real instance id example: (publish :path "/calendar" :function #'(lambda(req ent) (gwl-make-object req ent "calendar:assembly"))) <-Back Copyright © 2025 Genworks ® International .
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/8/index.html
Type: reference

```
than a real instance id example: (publish :path "/calendar" :function #'(lambda(req ent) (gwl-make-object req ent "calendar:assembly"))) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/4/index.html
Type: reference

```
Function: GWL:CLEAR-ALL-INSTANCES <-Back Function: Gwl:Clear-All-Instances CLEAR-ALL-INSTANCES void Clears all instances from GWL's master table of root-level instances. The instance IDs are the numbers you see in published GWL URIs, and are available as the "instance-id" message within each GWL object which inherit from base-html-sheet. Clearing all the instances makes available for garbage collection all memory used by the object hierarchies rooted at the instances, as well as all associated published URIs. example: (clear-all-instance) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/2/index.html
Type: reference

```
Function: GWL:BASE64-ENCODE-LIST <-Back Function: Gwl:Base64-Encode-List BASE64-ENCODE-LIST string Encodes a list into base64 without the trailing = signs. arguments: list list <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/15/index.html
Type: reference

```
Macro: GWL:WITH-CL-WHO <-Back Macro: Gwl:With-Cl-Who WITH-CL-WHO form Sets up body to be evaluated with cl-who and output the resulting string to the default *stream* Note that the args are spliced into cl-who:with-html-output after *stream* nil, so for example you can do (with-cl-who (:indent t) ...) and it will expand into: (with-html-output (*stream* nil :indent t) ...) . <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/5/index.html
Type: reference

```
Function: GWL:CLEAR-INSTANCE <-Back Function: Gwl:Clear-Instance CLEAR-INSTANCE void Clears the specified instance from GWL's master table of root-level instances. The instance ID is the same number you see in published GWL URIs, and is available as the "instance-id" message within all GWL objects which inherit from base-html-sheet. Clearing the specified instance makes available for garbage collection all memory used by the object hierarchy rooted at the instance, as well as all associated published URIs. arguments: id Integer or Keyword Symbol The key whose entry you wish to clear from the *instance-hash-table* example: (clear-instance 639) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/11/index.html
Type: reference

```
Function: GWL:PUBLISH-STRING-CONTENT <-Back Function: Gwl:Publish-String-Content PUBLISH-STRING-CONTENT string (representing a url path) Publishes given url to respond with text content as specified by given string. arguments: url String The url to be published string String The content to be emitted when the url is requested rest arguments: publish-args plist Arguments to be passed on to publish function, e.g. :content-type <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/13/index.html
Type: reference

```
Function: GWL::SESSION-CONTROL-AUTO-REFRESH <-Back Function: Gwl::Session-Control-Auto-Refresh SESSION-CONTROL-AUTO-REFRESH adding this javascript function into the header of a web page will cause the page to timeout and reload repeatedly This is intended to be used such that when an instance is open in an active browser the page will automatically update the expires-at function even if the operator takes an extended break from the application. It works by checking if any forms exist on this page. If they do it will submit the first form on the page when the timeout value is reached. This is done to avoid the Post Data confirmation warning that most browser present. If no forms are found it will use the reload(true) function to reload the page. arguments: timeout Time in seconds between page reloads optional arguments: html-stream Stream which the output should be sent to , Default Value: NIL Default is *html-stream* <-Back Copyright © 2025 Genworks ® International .
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/13/index.html
Type: reference

```
ge reloads optional arguments: html-stream Stream which the output should be sent to , Default Value: NIL Default is *html-stream* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/1/index.html
Type: reference

```
Function: GWL:BASE64-DECODE-SAFE <-Back Function: Gwl:Base64-Decode-Safe BASE64-DECODE-SAFE string Decodes a base64 string without need for trailing = signs into a decoded string. arguments: string string <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/17/index.html
Type: reference

```
Macro: GWL:WITH-HTML-FORM <-Back Macro: Gwl:With-Html-Form WITH-HTML-FORM enclose a body of code with a form . flag: -- fill in. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/3/index.html
Type: reference

```
Function: GWL:BASE64-ENCODE-SAFE <-Back Function: Gwl:Base64-Encode-Safe BASE64-ENCODE-SAFE string Encodes a string into base64 without the trailing = signs. arguments: string string <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/6/index.html
Type: reference

```
Function: GWL:CLEAR-OLD-TIMERS <-Back Function: Gwl:Clear-Old-Timers CLEAR-OLD-TIMERS void This is a lighter-weight alternative to the session-object-mixin for timing out instances in a web application. keyword arguments: idle-time-required Time in seconds , Default Value: 600 The maximum age of a session for timeout <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/12/index.html
Type: reference

```
Function: GWL::RELATIVIZE-PATHNAME <-Back Function: Gwl::Relativize-Pathname RELATIVIZE-PATHNAME Return a relative pathname for TARGET-PATHNAME that can be reached from the directory that TARGET-PATHNAME refers to. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/8/function-docs/dokumentation/9/index.html
Type: reference

```
Function: GWL:PUBLISH-GWL-APP <-Back Function: Gwl:Publish-Gwl-App PUBLISH-GWL-APP void Publishes an application, optionally with some initial arguments to be passed in as input-slots. arguments: path String The URL pathname component to be published string-or-symbol String or symbol The object type to insantiate keyword arguments: make-object-args Plist , Default Value: NIL Extra arguments to pass to make-object <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

