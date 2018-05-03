#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May  2 20:03:38 2018

@author: Alpoise
"""

import tensorflow as tf
import os

os.chdir('/Users/Alpoise/Desktop/DL_course/HW2')

import datetime
import loadData
from AlexNet import CNN
# Parameters
# ==================================================

# 模型超参
tf.flags.DEFINE_float("dropout_keep_prob", 0.5, "Dropout keep probability (default: 0.5)")
tf.flags.DEFINE_float("l2_reg_lambda", 0.01, "L2 regularization lambda (default: 0.01)")

# 训练参数
tf.flags.DEFINE_integer("batch_size", 30, "Batch Size (default: 30)")
tf.flags.DEFINE_integer("num_epochs", 20, "Number of training epochs (default: 20)")
tf.flags.DEFINE_integer("evaluate_every", 20, "Evaluate model on dev set after this many steps (default: 20)")

FLAGS = tf.flags.FLAGS
FLAGS._parse_flags()
print("\nParameters:")
for attr, value in sorted(FLAGS.__flags.items()):
    print("{}={}".format(attr.upper(), value))
print("")

# 数据载入
train_x, train_y, test_x, test_y = loadData.get_input()
print("Train/Test: {:d}/{:d}".format(len(train_y), len(test_y)))

# 训练
# ==================================================
with tf.Graph().as_default():
    sess = tf.Session()
    with sess.as_default():
        cnn =CNN(
            num_classes=17,
            l2_reg_lambda=FLAGS.l2_reg_lambda)

        # 定义训练op
        global_step = tf.Variable(0, name="global_step", trainable=False)
        optimizer = tf.train.AdamOptimizer(1e-4)
        grads_and_vars = optimizer.compute_gradients(cnn.loss)
        train_op = optimizer.apply_gradients(grads_and_vars, global_step=global_step)
        
        # 变量初始化
        sess.run(tf.global_variables_initializer())

        def train_step(x_batch, y_batch):
            feed_dict = {
              cnn.input_x: x_batch,
              cnn.input_y: y_batch,
              cnn.dropout_keep_prob: FLAGS.dropout_keep_prob
            }
            _, step, loss, accuracy = sess.run(
                [train_op, global_step,cnn.loss, cnn.accuracy],
                feed_dict)
            time_str = datetime.datetime.now().isoformat()
            print("{}: step {}, loss {:g}, acc {:g}".format(time_str, step, loss, accuracy))

        def test_step(x_batch, y_batch):
            feed_dict = {
              cnn.input_x: x_batch,
              cnn.input_y: y_batch,
              cnn.dropout_keep_prob: 1.0
            }
            step,loss, accuracy = sess.run(
                [global_step,cnn.loss, cnn.accuracy],
                feed_dict)
            time_str = datetime.datetime.now().isoformat()
            print("{}: step {}, loss {:g}, acc {:g}".format(time_str, step, loss, accuracy))

        # 生成小batch
        batches =loadData.batch_iter(
            list(zip(train_x,train_y)), FLAGS.batch_size, FLAGS.num_epochs)
        # 对每个batch循环：
        for batch in batches:
            x_batch, y_batch = zip(*batch)
            train_step(x_batch, y_batch)
            current_step = tf.train.global_step(sess, global_step)
            if current_step % FLAGS.evaluate_every == 0:
                print("\nEvaluation:")
                test_step(test_x, test_y)
            