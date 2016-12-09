#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: myLR.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-06 22:01:24
#
# Last   modified: 2016-12-06 22:01:26
#
#
#
rm(list = ls())
source('head.R')
data(iris)
iris$Species <- as.numeric(iris$Species)
iris$Species[iris$Species != 1] = 2

x = iris[,-5]
y = iris[,5]

sigmoid <- function(x,theta){
  return(1/(1 + exp(-x%*%theta)))
}

cost <- function(pred,y){
  m <- length(y)
  return((sum(-log(pred[y == 1])) + sum(-log(1 - pred[y == 0])))/m)
}

gradient <- function(x,y,theta){
  
}

myLR <- function(x,y){
  x <- as.matrix(cbind(1,x))
  len_col <- dim(x)[2]
  len_row <- dim(x)[1]
  theta <- matrix(rep(0,len_col),len_col,1)
  
  while(1){
    pred <- sigmoid(x,theta)
    cost <- myCost(pred,y)
  }
  
  print(cost)
}

myLR(x,y)