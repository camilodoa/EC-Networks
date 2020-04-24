# Deep Evolutionary Image Generation

## About

We are evolving a population of images according to the criteria of a deep CNN classifier trained on a MNIST dataset. Our goal is producing artificial images that fool high-accuracy classifiers.

Our evolutionary algorithm is found in `src/deig/core.clj`. The fitness function `src/deig/fitness.clj` is based on `fitness.py`. Our mnist model is trained with `mnist.py` and saved to `mnist.h5`.

## Installation

You'll need the following software to run our system.

#### Clojure and Leinigen

You can either download the Clojure interpreter, or you can use IntelliJ IDEA with the Cursive plugin.

If you want to go the terminal route, here is what you should run:

```bash
brew install clojure # Download Clojure interpreter
brew install leiningen # Download Leinigen project manager
lein deps # Install our clojure dependencies
```

#### Python3

You'll also need Python3 to train a new mnist classifier and to use our fitness function.

```bash
brew install python3 # Download Python3 interpreter
pip3 install -r requirements.txt # Install our python dependencies
```

## Running

If you want **to train a new classifier model**, do

```bash
python3 mnist.py
```

This will save a new `mnist.h5` that our evolutionary algorithm will use for its fitness. To change the model that gets trained, edit the code in `mnist.py`. If you want to test your model on a sample image after training it, run `fitness.py`. This will use your model to predict `test.jpg`'s classes.


To **run our genetic algorithm**, do

```bash
lein run # Runs (-main) in deig.core
```

This will run our genetic algorithm with a preset population and generation cap. If you want to edit these parameters, change `(-main)` in `src/deig/core.clj`. High-fitness individuals are saved in `individuals/`.

## Getting started with git

### Vanilla git

First, install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) so you can access it in your shell (alternatively you can use a git-helper application like [GitHub Desktop](https://desktop.github.com/)). If you have a Mac, I would recommend installing [Homebrew](https://brew.sh/), and then running

```bash
brew install git
```

Homebrew takes care of your paths and is overall super convenient.

Then, navigate to the directory where you would like to place this project folder. Once you've done this, run

```bash
git clone https://github.com/camilodoa/EC-Networks.git
```

to clone the repo locally.

If you want to push your changes to GitHub, you do:

```bash
git add .
git commit -m "your message"
git push
```

For simple merging you want to do:
```bash
# In the merge message do:
# ESC
# :wq ENTER
git push
```

### git-lfs

You'll need Git Large File Storage. [Install](https://git-lfs.github.com/) it.

Then, run

```bash
git lfs install
```

[This](https://medium.com/@AyunasCode/how-to-push-large-files-to-github-253d05cc6a09) is a good tutorial for how to commit really large files.


## Notes

Notes people take are put in the `/text` directory.

## Communication

We have a [Slack channel](https://join.slack.com/t/ec-networks/shared_invite/zt-d2zlhyvq-0nHuia~~UffdUTl8EGBUGg).
