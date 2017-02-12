<template>
  <div class="hello">
    <h1>{{ msg }}</h1>
    <a v-for="link in links"
       :href="link.href"
       :title="link.text + ' page'"
       @click="clickedLink">{{ link.text }}</a>
  </div>
</template>

<script>
export default {
  name: 'hello',
  data () {
    return {
      msg: 'Hello, World!',
      links: [
        { href: '#/profile', text: 'Profile' },
        { href: '#/contact', text: 'Contact' }
      ],
      clickedLink: function (e) {
        console.log('Going to: ' + e.target.href)
      }
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
a {
  text-decoration: none;
}

a:nth-of-type(n+2):before {
  content: "-";
  margin: 0 10px;
}

.hello {
  text-align: center;
}
</style>
