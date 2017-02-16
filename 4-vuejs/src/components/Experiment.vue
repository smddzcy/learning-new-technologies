<template>
  <div>
    <h1>{{ heading }}</h1>

    <div>
      <input type="text" v-model="msg" />
      <div>Computed: {{ computedMsg }}</div>
      <div>Method: {{ reversedMsg() }}</div>
    </div>

    <div style="margin-top: 20px">
      <!-- .stop => Stop propogation - .prevent => Prevent default -->
      <button type="button" @click.stop.prevent="httpReq($event)">Send Request</button>
      <pre v-bind:class="{ activePre: httpSuccess }">{{ httpResp }}</pre>
    </div>

    <router-link to="/" class="go-home-link">Go Home</router-link>
  </div>
</template>

<script>
import axios from 'axios'

export default {
  name: 'experiment', // Allows to instantiate the component in itself
  data: function () {
    return {
      heading: 'Experimenting Stuff',
      msg: 'Test string',
      httpResp: ''
    }
  },
  computed: {
    computedMsg: function () {
      return this.msg.split('').reverse().join('')
    }
  },
  methods: {
    // Almost the same as using computed data,
    // but **computed properties are cached based on their dependencies.**
    // A method invocation will always run the function on re-render.
    reversedMsg: function () {
      return this.msg.split('').reverse().join('')
    },
    now: function () {
      return Date.now()
    },
    httpReq: function () {
      axios.get('http://localhost:8080/#/')
        .then(function (resp) {
          this.httpResp = resp.data
          this.httpSuccess = true
        }.bind(this))
        .catch(function (error) {
          console.log(error)
          this.httpResp = 'Error.'
          this.httpSuccess = false
        }.bind(this))
    }
  },
  watch: {
    msg: function (val) {
      console.log('msg is changed to: ' + val)
    }
  }
}
</script>

<style scoped>
.activePre {
  border-color: #42b983;
}
</style>
