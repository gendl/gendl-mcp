(in-package :training-g101)

(define-object this-course (slide-show-leaf)
  :computed-slots
  ((strings-for-display "This Course")
   (slide-data 
    `(      
      (:title
       "Goals of the course"
       :bullet-points
       ((:description
	"To provide a basic grounding in the tools and techniques used to deliver models using Gendl")
       (:description
	"To use a combination of examples, lecture material, discussion and exercises to provide the necessary understanding")
       (:description
	"To proceed at a pace which is suited to the student")
       (:description
	"To use self-defined examples where possible which are relevant to the students work place and representative of the types of problems they are working up to solving")
       (:description)
       (:description
	"The course material will be delivered remotely using internet video calls at an agreed frequency/schedule. Its expected these calls will be in the order of 1 hour duration although this is not  hard and fast limit. Support via email or messaging will also be provided")))

      (:title
       "Course structure"
       :bullet-points
       ((:description
	"1. Introductions and getting started with GDL including some example code and an overview of an example application")
       (:description
	"2. Basic tools <ul><li>Using EMACS (video + demo)</li>
                            <li>Using YADD</li>
                            <li>Using Geysr</li>
                            <li>Using REPL</li></ul>")
       (:description
	"3. The basics
                      <ul><li>LISP concepts</li>
                          <li>LISTS</li>
                          <li>NUMBERS and STRINGS</li>
                          <li>EQUALITY and COMPARISON</li></ul>")
       (:description
	"4. Tools for writing programmes 
                      <ul><li>CONTROL and ITTERATION</li>
                          <li>FUNCTIONS</li></ul>")
       (:description
	"5. Practical example 
                     <ul><li>revisiting the code used in 1.</li>
                         <li>More on positioning</li>
                         <li>Collecting information from objects into lists</li>
                         <li>:functions and defun's</li></ul>")
       (:description
	"6. Input and Output, including some detailed examples and use of the <i>format</i> directive")

       (:description
	"7. Practical example Extending code from 5. 
                  <ul><li>to read inputs from a file</li>
                      <li>to write outputs to file</li></ul>")
       
       (:description
	"8. Arrays and Hash Tables and a first look at optimisation. Symbols and working with packages")
       (:description
	"9. Practical example Extending code from 7.
                  <ul><li>to use Hash Tables and Arrays</li>
                      <li>to organise the code in packages</li></ul>")
       (:description
	"10. Summary, including discussion on key takeaways and main learning points, plus the scoping of a mini-project for the 
student to undertake to consolidate this first section of training
<Ul><li>2 extra sessions covering, design, review and general support</li></ul>")
       (:description
	"11. Mini Project review - solution design and implementation")))))))
