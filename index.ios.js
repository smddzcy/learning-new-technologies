import React, {Component, PropTypes} from 'react';
import {
  AppRegistry,
  StyleSheet,
  View,
  Text,
  NavigatorIOS,
  TouchableOpacity
} from 'react-native';

export default class SimpleNavigationApp extends Component {
  handleNavigationRequest() {
    this.refs.nav.push({
      component: MyScene,
      title: 'Scene Title - Next Button',
      passProps: { text: 'Next scene' }
    });
  }

  render() {
    return (
      <NavigatorIOS
        ref='nav' // To access from refs
        initialRoute={{
          component: MyScene,
          title: 'Scene Title - Initial',
          passProps: {text: 'Initial scene' },
          rightButtonTitle: 'Next',
          onRightButtonPress: this.handleNavigationRequest.bind(this)
        }}
        style={{flex: 1}}/>
    );
  }
}

class MyScene extends Component {
  static propTypes = {
    text: PropTypes.string.isRequired,
    navigator: PropTypes.object.isRequired,
  }

  onForward = () => {
    this.props.navigator.push({
      component: MyScene,
      title: 'Scene Title - Button Access',
      passProps: {
        text: 'Scene accessed from button'
      }
    });
  }

  render() {
    return (
      <View style={{flex: 1, justifyContent: 'center', alignItems: 'center'}}>
        <Text>Current Scene: { this.props.text }</Text>
        <TouchableOpacity onPress={this.onForward}
                            activeOpacity={0.4}>
          <Text>Tap me to load the next scene</Text>
        </TouchableOpacity>
      </View>
    )
  }
}

AppRegistry.registerComponent('AwesomeProject', () => SimpleNavigationApp);
