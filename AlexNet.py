#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 23 12:55:45 2018

@author: Alpoise
"""

import tensorflow as tf

def conv(input,fh,fw,n_out,name):
    #第一层卷积用此函数，区别在于stride为4
    n_in = input.get_shape()[-1].value
    with tf.name_scope(name) as scope:
        filter = tf.get_variable(scope+"w", shape=[fh, fw, n_in, n_out], dtype=tf.float32,
                                 initializer=tf.contrib.layers.xavier_initializer_conv2d())
        #变量声明和初始化都打包在函数内
        conv = tf.nn.conv2d(input, filter, [1,4,4,1], padding='SAME')
        bias_val = tf.constant(0.0, shape=[n_out], dtype=tf.float32)
        biases = tf.Variable(bias_val, trainable=True, name='b')
        z = tf.nn.bias_add(conv, biases)
        relu = tf.nn.relu(z, name=scope)
        return relu

def conv_layer(input,fh,fw,n_out,name):
    #对隐藏层不使用stride
    n_in = input.get_shape()[-1].value
    with tf.name_scope(name) as scope:
        filter = tf.get_variable(scope+"w", shape=[fh, fw, n_in, n_out], dtype=tf.float32,
                                 initializer=tf.contrib.layers.xavier_initializer_conv2d())
        conv = tf.nn.conv2d(input, filter, [1,1,1,1], padding='SAME')
        bias_val = tf.constant(0.0, shape=[n_out], dtype=tf.float32)
        biases = tf.Variable(bias_val, trainable=True, name='b')
        z = tf.nn.bias_add(conv, biases)
        relu = tf.nn.relu(z, name=scope)
        return relu

def full_connected_layer(input,n_in,n_out,name):
    with tf.name_scope(name) as scope:
        filter = tf.get_variable(scope+"w", shape=[n_in, n_out], dtype=tf.float32,
                                 initializer=tf.contrib.layers.xavier_initializer_conv2d())
        bias_val = tf.constant(0.1, shape=[n_out], dtype=tf.float32)
        biases = tf.Variable(bias_val, name='b')
        fc = tf.nn.relu(tf.matmul(input,filter)+biases)
        return fc

def max_pool_layer(input,name):
    return tf.nn.max_pool(input, ksize=[1,3,3,1], strides=[1,2,2,1],padding='SAME', name=name)

def norm(name, l_input, lsize=4):
    return tf.nn.lrn(l_input, lsize, bias=1.0, alpha=0.001 / 9.0, beta=0.75, name=name)

class CNN(object):
    def __init__(
      self, num_classes,l2_reg_lambda=0.01):

        self.input_x = tf.placeholder(tf.float32, [None, 224,224,3], name="input_x")
        self.input_y = tf.placeholder(tf.float32, [None, num_classes], name="input_y")
        self.dropout_keep_prob = tf.placeholder(tf.float32, name="dropout_keep_prob")
        l2_loss = tf.constant(0.1)

        with tf.name_scope("conv-maxpool"):
            # First layer
            self.conv1_1 = conv(self.input_x,fh=11, fw=11,n_out=96,name="conv1_1")
            self.pool1 = max_pool_layer(self.conv1_1,name='pool1')
            self.norm1 = norm('norm1', self.pool1, lsize=4)
            self.norm1 = tf.nn.dropout(self.norm1, self.dropout_keep_prob)

            # Second layer
            self.conv2_1 = conv_layer(self.norm1, fh=5, fw=5, n_out=192,name='conv2_1')
            self.pool2 = max_pool_layer(self.conv2_1, name='pool2')
            self.norm2 = norm('norm2', self.pool2, lsize=4)
            self.norm2 = tf.nn.dropout(self.norm2, self.dropout_keep_prob)

            # Third layer
            self.conv3_1 = conv_layer(self.norm2, fh=3, fw=3, n_out=384, name='conv3_1')
            self.conv3_2 = conv_layer(self.conv3_1,fh=3, fw=3, n_out=384, name='conv3_2')
            self.conv3_3 = conv_layer(self.conv3_2, fh=3, fw=3, n_out=256, name='conv3_3')
            self.pool3 = max_pool_layer(self.conv3_3, name='pool3')
            self.norm3 = norm('self.norm3', self.pool3, lsize=4)
            self.norm3 = tf.nn.dropout(self.norm3,self.dropout_keep_prob)
            
        # Dense Layer <L2 for MLP>
        with tf.name_scope("output"):
            shape = self.norm3.get_shape().as_list()
            self.dim = 1
            for d in shape[1:]:
                self.dim *= d
            self.h_pool_flat= tf.reshape(self.norm3, [-1, self.dim])
            
            self.fc1 = full_connected_layer(self.h_pool_flat,n_in=self.dim,n_out = 4096,name = "fc1")
            self.fc2 = full_connected_layer(self.fc1,n_in = 4096,n_out = 1000,name = "fc2")
            self.fc3 = full_connected_layer(self.fc2,n_in = 1000,n_out = 17,name = "fc3")
            
            self.out = self.fc3

        self.loss, l2_loss = 0,0

        with tf.name_scope("loss"):
            losses = tf.nn.softmax_cross_entropy_with_logits(logits=self.out, labels=self.input_y)
            self.loss = tf.reduce_mean(losses)+l2_reg_lambda*l2_loss

        # Accuracy
        self.accuracy=0
        with tf.name_scope("accuracy"):
            correct_predictions = tf.equal(tf.argmax(self.out,1), tf.argmax(self.input_y, 1))
            self.accuracy = tf.reduce_mean(tf.cast(correct_predictions, "float"), name="accuracy")