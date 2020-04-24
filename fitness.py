'''
This code is the python version of fitness.clj
'''

# Utility imports for the example
from discriminator import Discriminator
import os
import random

# Imports our Clojure code will have to use
import numpy as np

from keras_preprocessing import image
from tensorflow.keras.models import load_model


def fitness(img):
    '''
    Our fitness function to be copied to Clojure
    '''
    x = image.img_to_array(img)
    x = np.expand_dims(x, axis=0)

    model = load_model('./mnist.h5')

    # This prediction variable is not used in fitness calculation
    prediction = model.predict_classes(x)
    print('Prediction is ', prediction)

    probabilities = model.predict_proba(x, verbose=1)[0]
    print('Probabilities are', probabilities)

    predictions = model.predict(x)
    print('New prediction is', predictions)

    # Fitness is calculated 0-100, where 100 is perfect and 0 is very bad
    fitness = max(probabilities) * 100
    print('Fitness is', fitness)
    return fitness

if __name__ == '__main__':
    'Sample usage'

    # Load file from HD
    img = image.load_img('test.jpg',
                        target_size=(28, 28), color_mode="grayscale")

    # Pass image to our fitness function
    fitness(img)
