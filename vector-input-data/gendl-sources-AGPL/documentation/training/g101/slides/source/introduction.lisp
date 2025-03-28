;;
;; Copyright 2002, 2009 Genworks International
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

(in-package :training-g101)

(define-object introduction (slide-show-leaf)
  :computed-slots
  ((strings-for-display "Introduction")
   (slide-data 
    `(      
      (:title 
       "About Me"
       :bullet-points
       ((:description "I live in the middle of the UK. I have a daughter who is currently studying at Tuscon University in Baltimore and a son currently studying for his pre-university exams. I'm a keen cyclist and currently reigning double world masters track champion")
	(:description
	 "I have a First Class Honours degree in Mechanical Engineering with Modern Language, and a Masters Degree in Manufacturing, specialising in CAD-CAM")
	(:description
	 "Started working with ICAD in 1998")
	(:description
	 "Won KBO World Prize for Innovation in KBE in 2000")
	(:description
	 "Lead a project with Jaguar to fully automate and optimise the engineering pocess for the design of pedestrian safe Hoods using 
                 <ul><li>i-DEAS (CAD A Class surface)</li>
                     <li>LS-DYNA (non-linear FEA)</li>
                     <li>NASTRAN (linear FEA)</li>
                     <li>Hypermesh (AutoMeshing)</li>
                     <li>ST-ORM (Stochasic Optimiser)</li>
                     <li>ICAD (KBE surface design, automesh post processing and general Glue-ware)</li></ul> 
           A project which arrived 15 years before people were ready to believe it could be done...
           <br>Read more about it at <a href=\"https://www.yumpu.com/en/document/read/5118897/kbe-surface-modeller-carhs\">
           https://www.yumpu.com/en/document/read/5118897/kbe-surface-modeller-carhs</a>")
	(:description
	 "One of the early adopters of GDL, scaling up a team to tackle Engineering and Business solutions, in the process making it to the 
          finals of the TATA Innovista awards for an Engineering Based Materials Selector application")
	(:description
	"Architected and built a 128-node High Performance Computing environment in 2005 with built in redundancy, high availability and priority based queue systems principally for running vehicle crash simulations. All performance reporting was custom built...using GDL")
	(:description
	 "Working in Enterprise scale IT, doing highly integrated projects using SAP, Oracle, Mainframes and both in-house and commercial applications supporting Supply Chain and Manufacturing... And wishing I had something as flexible as GDL to solve the problems we encounter....")
	 (:description
	"Recently started working with Gendle to re-write an application I had written using Excel to perform the aerodynamic analysis of track cyclists. The rationale for moving to Gendl was to make it faster, more usable and more easily manageable/extensible")
	(:description 
	 "I'd class myself as a user of the technology. I'm and Engineer, not a computer scientist, so have built my own internal models of how I think things work which seems to give me a reasonable methodology for developing code. But sometimes I have to ask Dave when my models break down!")))
      
      (:title 
       "CL Timeline - Past"
       :bullet-points
       ((:description 
	 "Conceived in 1958")
	(:description 
	 "Professor John McCarthy")
	(:description 
	 "2nd oldest high-level computer language still in use")
	(:description 
	 "Still on the leading edge")
	(:description 
	 "<b>Designed to evolve</b>")
	(:description 
	 "Its been used for some really big aplications,like the Original Yahoo Store, designing the wing systems for the Airbus 380, control systems for Boeing and its even been in outer space, courtesy of NASA")))
      
      
      (:title 
       "One of the most popular extension languages:"
       :bullet-points
       ((:description "The Genworks GDL System" )
	(:description "Gnu Emacs")
	(:description "The ICAD System")
	(:description "AutoCAD (AutoLisp, VisualLisp)")
	(:description "DesignPower Design++")
	(:description "Game development (Nichimen Graphics, Mirai, Naughty Dog Software)")
	(:description "Sawfish window manager")))

      (:title 
       "The Original RAD Environment"
       :bullet-points
       ((:description 
	 "<b><u>R</u></b>apid <b><u>A</u></b>pplication <b><u>D</u></b>evelopment")
	(:description "Write a prototype faster <b>than the spec</b>")
	(:description "Prototype is a better spec <b>than a paper spec</b>")
	(:description "Fine-tune the resulting prototype into a production application")))

      
      (:title 
       "Lisp Thrives on Complex Problems"
       :bullet-points
       ((:description
	 "You can't completely specify something if you have never solved something
like it before - you have to experiment and prototype")
	(:description
	 "The Lisp environment is well-suited to supporting exploratory programming")
	(:description
	 "The Lisp environment is also well-suited to supporting VERY large
programs in a workable manner (e.g. the GDL System)")
	(:description
	 "I've taken on projects that I've no idea how to solve at the outset, simply because I know that the underlying toolkit I have available - GDL/Gendl and LISP - will allow me to solve that problem. Its allowed us to write code fast and to write fast code")))

	(:title 
	 "So what is GDL/Gendl?"
	 :bullet-points
	 ((:description
	   "I see it as an extensible environment equipped with the tools I need to solve complex problems")
	  (:description
	   "Gendl provides a library of objects which I can use directly or use as the basis for my own custom objects")
	  (:description
	   "It provides a library of generic functions I can use in the code I write")
	  (:description
	   "It provides me with a scalable, integrated means of providing a user interface to the solutions I deliver")
	   (:description
	    "It allows me to mix Gendle object and functions with libraries of Common Lisp objects and functions and to use Common Lisp or libray finctions or Gendle functions to deliver my own specific objects and functions all in a seemless way. In the main, its difficut to tell the different components apart")))
	  
	(:title 
	 "Useful References"
	 :bullet-points
	 ((:description "There are many reference books which can be useful resources, but 2 in particular stand out
                             <ul><li>ANSI Common Lisp by Paul Graham (Prentice Hall, ISBN 0-13-370875-6)                 
                                 <li>Practical Common Lisp by Peter Seibel (Apress, ISBN 1-59059-239-5)</li></ul>")
	  (:description "A succinct online reference is avaiable at 
                         <a href=\"https://jtra.cz/stuff/lisp/sclr/index.html\">https://jtra.cz/stuff/lisp/sclr/index.html</a> 
                         and incidentally demonstrates just how compact Common Lisp is")
	  (:description "Once you have started Gendl, online documentation will be available at 
                         <a href=\"http://localhost:9000/yadd\">http://localhost:9000/yadd</a>")
	  (:description "Finally, Gendle uses the cl-who library for generating html when doing custom web pages. cl-who 
                         documentation is available at <a href=\"https://edicl.github.io/cl-who/\">https://edicl.github.io/cl-who/</a> 
                         but a word of warning, its not for the beginner!")))))))
