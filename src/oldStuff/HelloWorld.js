
import React, {Component} from 'react';
import {
  Text,
  View,
  StyleSheet,
  TextInput,
  ListView
} from 'react-native';

class AwesomeInput extends Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      text: ''
    };
  }

  toNoobCase(text) {
    const map = {
      'o': '0',
      'a': '4',
      '1': '!',
      'i': '!',
      's': '5',
      'e': '3'
    };
    for (key in map) {
      text = text.replace(key, map[key]);
    }
    return text;
  }

  onChangeText(newText) {
    this.setState({text: newText.toLowerCase()});
  }

  render() {
    return (
      <View style={{
        padding: 10
      }}>
        <TextInput
          style={{
          height: 40
        }}
          placeholder="Noobify"
          onChangeText={this
          .onChangeText
          .bind(this)}/>
        <Text style={{
          padding: 10
        }}>
          {this.toNoobCase(this.state.text)}
        </Text>
      </View>
    );
  }
}

class List extends Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      style: styles.listElement
    }
  }

  log(e) {
    console.log('Pressed!');
    console.log(e);
  }

  onLongPress() {
    console.log('Long press!');
  }

  render() {
    return (
      <View style={styles.list}>
        {this
          .props
          .elements
          .map((el, i) => {
            return (
              <Text
                style={this.state.style}
                onPress={this.log}
                key={i}
                onLongPress={this
                .onLongPress
                .bind(this)}>• {el}</Text>
            );
          })}
      </View>
    );
  }
}

export default class MainView extends Component {
  render() {
    const listEls = ["Wow, custom list element.", "Wow, custom list element 2.", "Wow, custom list element 3."];
    return (
      <View style={styles.container}>
        <Text style={styles.welcome}>
          Hello, World!
        </Text>

        <AwesomeInput/>

        <List elements={listEls}/>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF'
  },
  welcome: {
    fontSize: 20,
    textAlign: 'center',
    margin: 10
  },
  instructions: {
    textAlign: 'center',
    color: '#333333',
    marginBottom: 5
  },
  list: {
    flex: 0
  },
  listElement: {
    textAlign: 'left',
    color: '#333333',
    marginBottom: 5,
    padding: 5,
    backgroundColor: '#fafafa',
    borderColor: '#e1e1e1',
    borderWidth: 1,
    borderStyle: 'dashed'
  }
});
