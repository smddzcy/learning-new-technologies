# TensorFlow

> TensorFlow is an open source software library for numerical computation using data flow graphs.

I start with only a little basic machine learning knowledge. It will be a long and hard learning process - basically, it'll never end.

## Steps

### Installation (macOS)

- Install pip, then virtualenv: `easy_install pip && pip install --upgrade virtualenv`
- Create a virtualenv environment for TensorFlow: `virtualenv --system-site-packages ~/tensorflow`
- Activate the virtualenv environment: `source ~/tensorflow/bin/activate`
- Install TensorFlow (Python 2.7, CPU): `pip install --upgrade tensorflow` - if you want GPU support (needs NVIDIA CUDA GPU): `pip install --upgrade tensorflow-gpu`
