<template>
<div id="app">
  <router-view></router-view>
</div>
</template>

<script>
export default {
  name: 'app'
}
</script>

<style>
* {
  box-sizing: border-box;
}

html,
body {
  margin: 0;
  padding: 0;
  min-width: 100%;
  width: 100%;
  max-width: 100%;
  min-height: 100%;
  height: 100%;
  max-height: 100%;
}

#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

hr {
  border-top: 0;
  border-bottom: 1px solid rgba(44, 62, 80, 0.5);
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: normal;
}

a {
  color: #42b983;
}

input,
textarea,
button,
pre {
  border: 1px solid #ccc;
  background-color: #fafafa;
}

input,
textarea {
  padding: 5px;
}

button,
pre {
  padding: 10px;
}

.go-home-link {
  float: right;
  margin-top: 1rem;
}
</style>
