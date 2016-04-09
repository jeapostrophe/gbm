#lang racket/base
(require "r.rkt")

;; XXX use doxygen's XML output to get thhe definitions?

(define <GLFW/glfw3.h>
  (r-include
   #:pre-options '("#define GLFW_INCLUDE_GLCOREARB"
                   "#pragma clang diagnostic push"
                   "#pragma clang diagnostic ignored \"-Wdocumentation\"")
   "<GLFW/glfw3.h>"
   #:post-options '("#pragma clang diagnostic pop")))

(define-r-fns
  #:from <GLFW/glfw3.h>
  glfwInit glfwWindowHint)

(define-r-vals
  #:from <GLFW/glfw3.h>
  GLFW_SAMPLES
  GLFW_CONTEXT_VERSION_MAJOR
  GLFW_CONTEXT_VERSION_MINOR
  GLFW_RESIZABLE GL_FALSE
  GLFW_OPENGL_FORWARD_COMPAT GL_TRUE
  GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)

(provide (all-defined-out))
