import tensorflow as tf

sess = tf.Session()

# create 2 constant tf nodes
one = tf.constant(1.0, tf.float32)
two = tf.constant(2.0)  # calls tf.float32 implicitly

# create a computation node
add_one_and_two = one + two  # same as tf.add(one, two)

print("sess.run(add_one_and_two): ", sess.run(add_one_and_two))

# use placeholders for non-constant operations
# a = tf.placeholder(tf.float32)
# b = tf.placeholder(tf.float32)
# adder_node = a + b
#
# print(sess.run(adder_node, { a: 3, b: 4 }))
# print(sess.run(adder_node, { a: [3.0, 4.0], b: [4.0, 5.0] }))

# let's create a linear model
a = tf.Variable([.5], tf.float32)
b = tf.Variable([-.5], tf.float32)
x = tf.placeholder(tf.float32)

linear_model = a * x + b

# constants are initialized when you call tf.constant,
# and their value can never change.
# variables are __not__ initialized when you call tf.Variable.
# to initialize all the variables explicitly call:
init = tf.initialize_all_variables()
sess.run(init)

print(sess.run(linear_model, {x: [1, 2, 3, 4]}))
