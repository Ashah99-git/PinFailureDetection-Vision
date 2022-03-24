import cv2 as cv
import numpy as np
import os
import matplotlib.pyplot as plt

# import tensorflow as tf


counts = 0

workingDirectory  = os.getcwd()

cfg_file = os.path.join( workingDirectory, './src/yolov4_dye_pry_custom_test.cfg')
weight_file = os.path.join( workingDirectory, './src/yolov4_dye_pry_custom_train_final.weights')



indexes_1 = []
boxes_1 = []

# data = []

classes = ['Pins']


net = cv.dnn.readNetFromDarknet(cfg_file, weight_file)

def reset_files(path):
  
    path1 = path.replace("0.jpg", "") 
    os.chdir(path1)
    all_files = os.listdir()
    for f in all_files:
      os.remove(f)


def yolo_detection(path):
    flag_i = 0
    # global img
    #img = cv.imread(r'C:\\Users\\rsran\\AppData\\Local\\Temp\\RtmpgnLOQ3/0889071a9c5e42722bc56ed0/0.jpg')

    #img = cv.imread('r',path)
    img = cv.imread(path)
    height = np.size(img, 0)
    width = np.size(img, 1)

    height, width, _ = img.shape
    blob = cv.dnn.blobFromImage(img, 1 / 255, (416, 416), (0, 0, 0), swapRB=True, crop=False)

    net.setInput(blob)

    output_layers_name = net.getUnconnectedOutLayersNames()

    layerOutputs = net.forward(output_layers_name)

    boxes = []
    confidences = []
    class_ids = []

    for output in layerOutputs:
        for detection in output:
            score = detection[5:]
            class_id = np.argmax(score)
            confidence = score[class_id]
            if confidence > 0.7:
                center_x = int(detection[0] * width)
                center_y = int(detection[1] * height)
                w = int(detection[2] * width)
                h = int(detection[3] * height)
                x = int(center_x - w / 2)
                y = int(center_y - h / 2)
                boxes.append([x, y, w, h])
                confidences.append((float(confidence)))
                class_ids.append(class_id)

    indexes = cv.dnn.NMSBoxes(boxes, confidences, .5, .4)

    for output in layerOutputs:
        for detection in output:
            score = detection[5:]
            class_id = np.argmax(score)
            confidence = score[class_id]
            if confidence > 0.5:
                # ctr = ctr+1
                center_x = int(detection[0] * width)
                center_y = int(detection[1] * height)
                w = int(detection[2] * width)
                h = int(detection[3] * height)

                x = int(center_x - w / 2)
                y = int(center_y - h / 2)

                boxes.append([x, y, w, h])
                confidences.append((float(confidence)))
                class_ids.append(class_id)

    indexes = cv.dnn.NMSBoxes(boxes, confidences, .8, .4)
    class_name = classes

    if len(indexes) > 0:
        for i in indexes.flatten():
            if flag_i == 0:  # for initialising the values x_min,x_max,y_min,y_max
                x_min, y_min, w, h = boxes[i]
                x_max, y_max, w, h = boxes[i]
                flag_i = 1

            x, y, w, h = boxes[i]
            if x < x_min:
                x_min = x
            if x > x_max:
                x_max = x
            if y < y_min:
                y_min = y
            if y > y_max:
                y_max = y
    global indexes_1
    indexes_1 = indexes
    # print(indexes_1)
    global boxes_1
    boxes_1 = boxes
    # print(boxes_1)
    return (x_min,x_max,y_min,y_max)

def well_pos(x_cod,y_cod):
    # print(indexes_1)
    #ex = 40 # give value which is appx half of bbox width
    if len(indexes_1) > 0:
          for i in indexes_1.flatten():
               x, y, w, h = boxes_1[i]
               if x <= (x_cod + (w/2)) and (x_cod + (w/2) <= x+w) and (y <= y_cod + (h/2)) and (y_cod + (h/2) <= y+h):
                   return x,y,w,h
    return 0

count = 0
def yolov4_m2(x,y,w,h, path, well_x, well_y):
    path1  = path
    global count
    #detection_st = 1
    #imgx = cv.imread(r'D:\Disk F\DIT\Winter Semester\Case study autonomous systems\R\53.jpg')
    imgx = cv.imread(path)
    cropped_img = imgx[int(y):int(y + h), int(x):int(x + w)]
    path1 = path.replace("0.jpg", "") 
    img_name = str( well_x) +'_'+ str(well_y ) + '.jpg'
    img_path = os.path.join(path1, img_name)
    cv.imwrite(img_path, cropped_img)
    count += 1
