# Pin Failure Detection using Computer Vision
Detection of Soldering failure by integrating DeepLearning techniques to the "Die and Pry" Experiment

## Approach

The approach to tackle this was to make this process as autonomous as possible. YOLO (You Only Look Once), which is a real time object detection algorithm that uses Convolutional Neural Networks (CNNs) to provide real time object detection. The YOLO model is trained to detect each pin in an image and create a boundary box around every pin.

- To make this process user friendly a GUI based on the R shiny platform was developed. The GUI lets users edit BGA schema according to the defects detected in dye and pry experiments as good or bad pins.

- To achieve this, a SQL database is created which stores all the BGA Grid and its dimensions. From this database, an editable grid can be created to help the user map all the good and bad pins detected from the dye and pry experiment.

- Users can also upload an image of the result from dye and pry experiments. YOLO model is applied to this image. Using this all the pins in the image are cropped into smaller images containing only a single pin. These images help the user to determine the status of pins as good or bad. Users can also hover over a box in the grid to see the preview of cropped images obtained using YOLO model.
- After users go through all the pins in the grid and all the boxes in the grid are marked as good or bad it can be saved as a pdf file. It contains the snapshot of the grid user created. Users also have an option to export it as a CSV file which contains the data in a tabular format for further use.

# Flow Chart 

![image](https://user-images.githubusercontent.com/102171203/159985580-9485f05c-81e7-45d4-bcba-3122911a927a.png)


# UI

<img width="300" alt="image" src="https://user-images.githubusercontent.com/102171203/159985997-a6af94ca-1a35-4ff8-abad-b19b70c1edfe.png">             <img width="350" alt="image" src="https://user-images.githubusercontent.com/102171203/159986073-ed717bad-ff13-48dc-9be0-295459bd6913.png">

<img width="350" alt="image" src="https://user-images.githubusercontent.com/102171203/159986117-30832c9d-9803-4501-ac13-723b76433712.png">
