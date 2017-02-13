<template>
  <div>
    <div class="success" v-if="messageSent">
      <div class="message">Your message has been sent successfully!</div>
      <router-link to="/" class="go-home-link">Go Home</router-link>
    </div>

    <div v-else>
      <h1 v-once>{{ heading }}</h1>

      <div class="fill-info">
        <div class="row">
          <h3>Name: </h3>
          <input type="text" v-model.trim="info.name" placeholder="Your name"/>
        </div>
        <div class="row">
          <h3>Email: </h3>
          <input type="email" v-model="info.email" placeholder="Your email"/>
        </div>
        <div class="row">
          <h3>Message: </h3>
          <textarea v-model.trim="info.message" placeholder="Your message"/>
        </div>
        <div class="row">
          <button type="submit" @click="onSubmitButtonClick">Submit</button>
        </div>
      </div>

      <router-link to="/" class="go-home-link">Go Home</router-link>
    </div>
  </div>
</template>

<script>
export default {
  name: 'profile',
  data: function () {
    return {
      heading: 'Contact Page',
      info: {
        name: '',
        email: '',
        message: ''
      },
      onSubmitButtonClick: () => {
        console.log(this.$data.messageSent)
        this.$data.messageSent = true
      },
      messageSent: false
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

.success {
  width: 240px;
  height: 130px;
  padding: 20px;
  border: 1px solid #42b983;
  border-radius: 10px;
  display: flex;
  flex-flow: column nowrap;
  align-items: center;
  justify-content: center;
  text-align: center;
}
</style>
