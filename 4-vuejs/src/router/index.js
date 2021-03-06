import Vue from 'vue'
import Router from 'vue-router'
import Hello from 'components/Hello'
import Profile from 'components/Profile'
import Contact from 'components/Contact'
import Experiment from 'components/Experiment'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'Hello',
      component: Hello
    }, {
      path: '/profile',
      name: 'Profile',
      component: Profile
    }, {
      path: '/contact',
      name: 'Contact',
      component: Contact
    }, {
      path: '/experiment',
      name: 'Experiment',
      component: Experiment
    }
  ]
})
