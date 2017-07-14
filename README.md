# p3
This is an implementation of Project3, course VE280 (Joint Institute) *in Haskell*.

Anyone kindly keeping an eye on this repo should be aware that this is a **heavy WIP**. @tonyfloatersu and myself are working to finalize it. This repo, being our register for solving various problems in the project (explained below), will be subject to constant change for a period of time, afterwards which will be explicitly announced.

## Briefly on the Project
File `Project3-Description.pdf` is a lengthy but detailed specification of the project.

## Problem List
One could imagine according to the PDF that the program will be about *modification*, and will hold *deep* data structure. I would not go to the length to say Haskell is not good at these, yet in that case more development experience is required compared to other languages. 

We're facing various problems that could, in abstract, sum into two:

* There is no *reference* data type in Haskell. In practice one may establish very complex data record, such as:
  ```
  data Course = Course Category CourseNum
  data CourseGPARec = GPARec Course Credit GPA
  data Student = Student Name [CourseGPARec]
  data JIGrade = Grade Year Map StuID Student
  data JIStudents = JI [JIGrade]
  ```

  So on, and so forth. Assume that we have an instance `students` of `JIStudents`, and we want to modify the GPA of a certain course taken by XXX in grade xxxx with student ID xxx. Then we certainly need to go deep into some field of `students`, to get the GPA, and pass it to a function producing new GPA, which is same as in C++. 
  
  Yet due to the data immutability in Haskell, what we do is to catch the new value, and rebuild the whole `JIStudents` data leaving other data the same as old, to change merely a GPA; though GHC does opt this for CPU's performance, developers' performance remains low because of the spaghetti code.

* We are facing a project that is artifically minimized, but actually large in framework. In especial, error detection is reduced to limited cases. 
  
  What is a merit in C++ does not work out samely favorable in Haskell. Haskell is a language that forces error checking, pulling you by type system and exception-expressive utility such as `Maybe` and `Either`, and pushing you by mystifying some behavior including `IO` and `catch`. 
  
  Hence the C++ version is simple, but leaking everywhere, while Haskell version could be quite robust, but is very elaborated.

## About Motivation and Development

Despite this project itself not requiring much time to finish, we encourage anyone with merest Haskell knowledge to show up and collaborate with us in future, and anyone simply intrigued to learn it together with us. 

In addition, this will be a non-HC approach to help people in future VE280 understand P3, since Haskell is a self-explanatory language. We'll also try to avoid obscure code, and ~~omit~~ increase comments in the code. 
