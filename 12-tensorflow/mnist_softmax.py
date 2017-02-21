"""A very simple MNIST classifier.

See extensive documentation at
http://tensorflow.org/tutorials/mnist/beginners/index.md
"""
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import sys

from tensorflow.examples.tutorials.mnist import input_data

import tensorflow as tf

FLAGS = None

def main(_):

  # Import data
  mnist = input_data.read_data_sets(FLAGS.data_dir, one_hot=True)

  # Create the model
  # https://www.tensorflow.org/images/softmax-regression-scalargraph.png
  #
  # Each image is 28 x 28, it's a 784-dimension vector if we flatten it
  # None means it can be any length - we want to put any number of images into x
  x = tf.placeholder(tf.float32, [None, 784])
  # Weights of pixels
  # 10 is because we want to multiply 784-dim image vector with it to produce
  # 10-dim vector of evidences
  W = tf.Variable(tf.zeros([784, 10]))
  # Biases
  b = tf.Variable(tf.zeros([10]))
  # Implement the model (without a softmax layer)
  y = tf.nn.softmax(tf.matmul(x, W) + b)

  # Placeholder for correct answers
  y_ = tf.placeholder(tf.float32, [None, 10])

  # Define loss ("cross-entropy") and optimizer
  #
  # Cross-entropy: H_{y'}(y) = -\sum_i y'_i \log(y_i)
  # y: predicted probability distribution
  # y': true distribution (the one-hot vector with the digit labels)

  # The raw formulation of cross-entropy:
  #   tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(tf.nn.softmax(y)),
  #                                 reduction_indices=[1]))
  # can be numerically unstable.
  #
  # So here we use tf.nn.softmax_cross_entropy_with_logits on the raw
  # outputs of 'y', and then average across the batch.
  #
  # labels: true values, y: predicted values
  cross_entropy = tf.reduce_mean(
      tf.nn.softmax_cross_entropy_with_logits(labels=y_, logits=y))
  # There are many optimizers:
  # https://www.tensorflow.org/api_guides/python/train#optimizers
  train_step = tf.train.GradientDescentOptimizer(0.5).minimize(cross_entropy)

  sess = tf.InteractiveSession()
  tf.global_variables_initializer().run()
  # Train!
  for _ in xrange(1000):
    # Ideally, we'd like to use all our data for every step of training
    # because that would give us a better sense of what we should be doing,
    # but that's expensive. So, instead, we use a different subset every time.
    # Doing this is cheap and has much of the same benefit.
    batch_xs, batch_ys = mnist.train.next_batch(100)
    sess.run(train_step, feed_dict={x: batch_xs, y_: batch_ys})

  # Test trained model
  #
  # tf.argmax(y, 1) is the label our model thinks is most likely for each input
  # tf.argmax(y_, 1) is the true ones
  correct_prediction = tf.equal(tf.argmax(y, 1), tf.argmax(y_, 1))
  # tf.cast turns [True False] into [1 0] so that we can get a numeric accuracy
  accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
  print(sess.run(accuracy, feed_dict={x: mnist.test.images,
                                      y_: mnist.test.labels}))

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('--data_dir', type=str, default='/tmp/tensorflow/mnist/input_data',
                      help='Directory for storing input data')
  FLAGS, unparsed = parser.parse_known_args()
  tf.app.run(main=main, argv=[sys.argv[0]] + unparsed)
