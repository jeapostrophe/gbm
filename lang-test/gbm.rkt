#lang racket/base
(require "r.rkt"
         "glfw.rkt")

(define main
  (r-fun ([r-int argc] [(r-ptr (r-ptr r-char)) argv]) : r-int
         (r-unless (glfwInit)
                   (stdlib:exit EXIT_FAILURE))
         (glfwWindowHint GLFW_SAMPLES 4)
         (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
         (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)
         (glfwWindowHint GLFW_RESIZABLE GL_FALSE)
         (glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT GL_TRUE)
         (glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
         (r-ret 0)))

(define exe
  (r-exe
   (r-public-fun "main" main)))

(module+ main
  (r-emit exe))
