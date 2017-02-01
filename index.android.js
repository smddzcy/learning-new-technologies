import React, { Component } from 'react';
import {
  AppRegistry,
  Text
} from 'react-native';

export default class AwesomeProject extends Component {
  render() {
    return <Text>Hello, World!</Text>;
  }
}

AppRegistry.registerComponent('AwesomeProject', () => AwesomeProject);
