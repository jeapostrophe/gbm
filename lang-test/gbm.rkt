#lang racket/base
(require "r.rkt"
         "glfw.rkt"
         (prefix-in stdlib: "stdlib.rkt"))

;; + Use simple queue (http://queues.io, such as nanomsg) for ITC

;; Triple Buffer http://remis-thoughts.blogspot.com/2012/01/triple-buffering-as-concurrency_30.html

;; + Threads:

;; Audio - Plays music through libsoundio

;; Graphics - Interacts with OpenGL/Vulkan and uses VSync

;; GUI - Runs the GLFW loop and copies input events into the buffer

;; Logic - Runs the game update function at 60Hz: first copies the
;; input buffer, then runs the update, then it copies the graphics
;; data and audio data to the appropriate places, ends by re-arranging
;; the pointers so that the next sync will get a fresh view of the RAM

;; (Eventually) Network - Coordinates with the network with a simple
;; ITC to the Game for looking for other games, updating their input,
;; and causing the Game to rewind

;; + Logic Structure:

;; ECS Engine - Update the systems in a certain sequence

;; ECS Concurrency ideas: See
;; Tharsis... http://ics.upjs.sk/~krajci/skola/ine/SVK/pdf/Majerech.pdf

;; ECS Threads - A set of libraries/macros that allow straight-line
;; code to be broken up into chunks that run as part of a system (one
;; for each yield block with the data from one control block to the
;; next being in the component for it)

;; Audio engine - Gathers samples to be played from ECS and then
;; simulates the various waveforms

(define main
  (r-fun ([r-int argc] [(r-ptr (r-ptr r-char)) argv]) : r-int
         (r-unless (glfwInit)
                   (stdlib:exit stdlib:EXIT_FAILURE))
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
