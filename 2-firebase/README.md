# Firebase

Firebase provides a realtime database and backend as a service.

## Steps

### Initial

- Install Firebase tools: `npm install -g firebase-tools`

- Login from CLI: `firebase login`

### Deployment

- Init the project: `firebase init`

- Init the Firebase app with the config:
```
<script src="https://www.gstatic.com/firebasejs/3.6.9/firebase.js"></script>
<script type="text/javascript">
  var config = {
    apiKey: "AIzaSyBR23al6Bhj-0av_IfEhpJp3OMR8o8y1L4",
    authDomain: "helloworld-db884.firebaseapp.com",
    databaseURL: "https://helloworld-db884.firebaseio.com",
    storageBucket: "helloworld-db884.appspot.com",
    messagingSenderId: "220377064236"
  };
  firebase.initializeApp(config);
</script>
```

- Write your awesome code:
```
// A post entry.
var postData = {
  body: body,
  title: title
};

// Get a key for a new Post.
var newPostKey = firebase.database().ref().child('posts').push().key;

// Write the new post's data simultaneously in the posts list and the user's post list.
var updates = {};
updates['/posts/' + newPostKey] = postData;

firebase.database().ref().update(updates);
```

- Deploy: `firebase deploy`
