<template>
  <div>
    <h1 v-once>{{ heading }}</h1>

    <div class="fill-info">
      <div class="row">
        <h3>Name: </h3>
        <input type="text" v-model.trim="info.name" placeholder="Your name"/>
      </div>
      <div class="row">
        <h3>Surname: </h3>
        <input type="text" v-model.trim="info.surname" placeholder="Your surname"/>
      </div>
      <div class="row">
        <h3>Age: </h3>
        <input type="number" v-model.number="info.age" placeholder="Your age"/>
      </div>
    </div>

    <hr>

    <div class="info">
      <h2>Your Info: </h2>

      <h3>{{ 'Name: ' + info.surname }}</h3><!--
   --><span v-if="info.surname && info.name">,</span>
      <h3>{{ info.name }}</h3>

      <br />

      <h3>{{ 'Age: ' + info.age }}</h3>
    </div>

    <router-link to="/" class="go-home-link">Go Home</router-link>
  </div>
</template>

<script>
export default {
  name: 'profile',
  data () {
    return {
      heading: 'Profile Page',
      info: {
        name: '',
        surname: '',
        age: ''
      }
    }
  }
}
</script>

<style scoped>
.fill-info > div.row {
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  justify-content: flex-start;
}

.fill-info > div.row input {
  height: 30px;
}

.fill-info > div.row h3 {
  margin-right: 10px;
  flex-basis: 100px;
}

.info h3 {
  display: inline;
}
</style>
