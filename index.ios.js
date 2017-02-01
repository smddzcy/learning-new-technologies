import React, {Component} from 'react';
import {
  AppRegistry,
  StyleSheet,
  Text,
  View,
  PixelRatio,
  TextInput,
  TouchableOpacity,
  ListView
} from 'react-native';
import {Socket} from 'phoenix';

const baseUrl = 'http://localhost:4000';

/// SOCKET START
let socket = new Socket(baseUrl + "/socket", {
  logger: ((kind, msg, data) => { console.log(`${kind}: ${msg}`, data) }),
  transport: WebSocket
});

socket.connect()

// Now that you are connected, you can join channels with a topic:
let channel = socket.channel("room:posts", {})
channel.join()
  .receive("ok", resp => { console.log("Joined successfully", resp) })
  .receive("error", resp => { console.log("Unable to join", resp) })
/// SOCKET END


async function postJSON(url, data) {
  try {
    let response = await fetch(url, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data)
    });
    return await response.json();
  } catch(err){
    console.error(err);
  }
}

async function getPosts() {
  try {
    let response = await fetch(baseUrl + '/posts');
    return (await response.json()).data;
  } catch(error) {
    console.error(error);
  }
}

/**
 * @param {String} title
 * @param {String} content
 */
async function createPost(title, content) {
  try {
    const data = {
      post: {
        title: title,
        content: content
      }
    };
    let response = await postJSON(baseUrl + '/posts', data);
    return response.data;
  } catch (error) {
    console.error(error);
  }
}

class CreatePostView extends Component {

  constructor(props, context) {
    super(props, context);

    // Set initial state
    this.state = {
      newPostTitle: null,
      newPostContent: null
    };
  }

  async onPostCreateButtonClick() {
    try {
      const resp = await createPost(this.state.newPostTitle, this.state.newPostContent);
      this.setState({
        newPostTitle: null,
        newPostContent: null
      });
    } catch(err) {
      console.error(err);
    }
  }

  render() {
    return (
      <View style={{flex: 1, marginBottom: 20}}>
          <Text style={styles.header}>Create Post</Text>

          <View style={{flexDirection: 'row'}}>
            <Text style={styles.h2}>Title: </Text>
            <TextInput placeholder='Title...'
                       style={{height: 24, width: 100}}
                       value={this.state.newPostTitle}
                       onChangeText={(text) => this.setState({newPostTitle: text})}/>
          </View>


          <View style={{flexDirection: 'row'}}>
            <Text style={styles.h2}>Content: </Text>
            <TextInput placeholder='Content...'
                       style={{height: 24, width: 100}}
                       multiline={true}
                       value={this.state.newPostContent}
                       onChangeText={(text) => this.setState({newPostContent: text})}/>
          </View>

          <View style={{flexDirection: 'row', alignItems: 'flex-start'}}>
            <TouchableOpacity onPress={this.onPostCreateButtonClick.bind(this)}
                              style={{paddingTop: 10, paddingBottom: 10, paddingRight: 10, backgroundColor: 'transparent'}}
                              activeOpacity={0.5}>
              <Text style={styles.buttonText}>Create</Text>
            </TouchableOpacity>
          </View>
      </View>
    );
  }
}


class MainView extends Component {
  constructor(props, context) {
    super(props, context);
    const ds = new ListView.DataSource({
      rowHasChanged: (r1, r2) => r1.id !== r2.id
    });

    this.state = {
      posts: [],
      dataSource: ds.cloneWithRows([])
    };

    channel.on('new:post', post => {
      let posts = this.state.posts;
      posts.push(post);

      this.setState({
        posts: posts,
        dataSource: ds.cloneWithRows(posts)
      });
    });

    getPosts().then((posts) => {
      this.setState({
        posts: posts,
        dataSource: ds.cloneWithRows(posts)
      });
    });
  }

  componentWillUnmount() {
    channel.off('new:post');
  }


  renderRow(data) {
    return (
      <View style={{marginBottom: 10}}>
        <Text><Text style={{fontWeight: 'bold'}}>ID:</Text> {data.id}</Text>
        <Text><Text style={{fontWeight: 'bold'}}>Title:</Text> {data.title}</Text>
        <Text><Text style={{fontWeight: 'bold'}}>Content:</Text> {data.content}</Text>
      </View>
    );
  }

  renderHeader(){
    return (
      <View>
        <Text style={styles.header}>Posts</Text>
      </View>
    )
  }

  render() {
    return (
      <View>
        <CreatePostView/>
        <ListView dataSource={this.state.dataSource}
                  renderRow={this.renderRow}
                  renderHeader={this.renderHeader}
                  enableEmptySections={true}/>
      </View>
    );
  }
}


export default class HelloWorldApp extends Component {
  render() {
    return (
      <View style={styles.container}>
        <MainView/>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF',
    paddingTop: 50
  },
  header: {
    fontSize: 24,
    marginBottom: 10,
    textDecorationStyle: 'solid',
    textDecorationColor: '#333333',
    textDecorationLine: 'underline'
  },
  h2: {
    fontSize: 18
  },
  buttonText: {
    fontSize: 16,
    color: "#1565C0"
  }
});

AppRegistry.registerComponent('AwesomeProject', () => HelloWorldApp);
