#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 23 12:56:57 2018

@author: Alpoise
"""

import os
import numpy as np
import scipy.io as sio
from PIL import Image

os.chdir('/Users/Alpoise/Desktop/DL_course/HW2')
dir='oxflower17/jpg/'

def to_categorical(y, nb_classes): #把label改成one_hot向量
    y = np.asarray(y, dtype='int32')
    if not nb_classes:
        nb_classes = np.max(y)+1
    Y = np.zeros((1, nb_classes))
    Y[0,y] = 1.
    return Y

def build_class_directories(dir):
    dir_id = 0
    class_dir = os.path.join(dir, str(dir_id))
    if not os.path.exists(class_dir):
        os.mkdir(class_dir)
    for i in range(1, 1361):
        fname = "image_" + ("%.4d" % i) + ".jpg"
        #%i改为了%d    
        os.rename(os.path.join(dir, fname), os.path.join(class_dir, fname))
        #把文件的位置重命名即意味着挪动了位置，比复制快
        if i % 80 == 0 and dir_id < 16:
            dir_id += 1
            class_dir = os.path.join(dir, str(dir_id))
            os.mkdir(class_dir)

def get_input(resize=[224,224]):
    print('Load data...')

    getJPG = lambda filePath: np.array(Image.open(filePath).resize(resize))
    #lamda简洁地定义函数的方式，冒号前面是自变量，后面是返回值
    #resize使得图片长宽统一，同时并不损失RGB值，参见Image包的用法
    dataSet=[];labels=[];choose=1
    classes = os.listdir(dir)
    for index, name in enumerate(classes):
        #enumerate将一个可遍历的数据对象组合为索引序列，同时列出数据和数据下标
        class_path = dir+ name + "/"
        if os.path.isdir(class_path): #只有分成组的图片才构成文件夹，才是一个路径
            for img_name in os.listdir(class_path):
                img_path = class_path + img_name #得到每张图的路径
                img_raw = getJPG(img_path) #读入每张图
                dataSet.append(img_raw) #append向列表尾部加一个元素
                y = to_categorical(int(name),17) #标记成one_hot label
                labels.append(y)

    datasplits = sio.loadmat('oxflower17/datasplits.mat')
    keys = [x + str(choose) for x in ['trn','val','tst']]
    #前面定义过choose=1，这里即选了datasplits的1子集
    train_set, vall_set, test_set = [set(list(datasplits[name][0])) for name in keys]
    #set函数创建成集合的形式，可并交补
    train_data, train_label,test_data ,test_label= [],[],[],[]

    for i in range(len(labels)):
        num = i + 1
        if num in test_set:
            test_data.append(dataSet[i])
            test_label.extend(labels[i])
        else:
            train_data.append(dataSet[i])
            train_label.extend(labels[i])
        #把索引在test_set的分到测试集，否则分到训练集
    train_data = np.array(train_data, dtype='float32')
    train_label = np.array(train_label, dtype='float32')
    test_data = np.array(test_data, dtype='float32')
    test_label = np.array(test_label, dtype='float32')
    return train_data, train_label,test_data ,test_label

def batch_iter(data, batch_size, num_epochs, shuffle=True):
    #1个epoch等于使用训练集中的全部样本训练一次
    data = np.array(data)
    data_size = len(data)
    num_batches_per_epoch = int((len(data)-1)/batch_size) + 1
    #重要！说明对每次epoch末尾不足一个batchsize的也算作一个batch，怎么算的见下文end_index处
    for epoch in range(num_epochs):
        # 每个epoch把数据打乱下顺序
        if shuffle:
            shuffle_indices = np.random.permutation(np.arange(data_size))
            shuffled_data = data[shuffle_indices]
        else:
            shuffled_data = data
        for batch_num in range(num_batches_per_epoch):
            start_index = batch_num * batch_size
            end_index = min((batch_num + 1) * batch_size, data_size)
            yield shuffled_data[start_index:end_index]
            #yield就是 return 返回一个值，并且记住这个返回的位置，下次迭代就从这个位置后开始

if __name__=='__main__':
    #build_class_directories(os.path.join(dir))
    #首次没有分类好的话运行此函数分类
    train_data, train_label,test_data, test_label=get_input()
    print(len(train_data),len(test_data))
#当模块被直接运行时，__name__ == '__main__'以下代码块将被运行，当模块是被导入时，代码块不被运行