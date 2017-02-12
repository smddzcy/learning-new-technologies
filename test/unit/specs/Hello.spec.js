import Vue from 'vue'
import Hello from 'src/components/Hello'

describe('Hello.vue', () => {
  const Constructor = Vue.extend(Hello)
  const vm = new Constructor().$mount()

  it('should render a correct heading', () => {
    expect(vm.$el.querySelector('.hello h1').textContent)
      .to.equal('Hello, World!')
  })

  it('should render the links', () => {
    expect(vm.$el.querySelectorAll('.hello a').length > 0)
      .to.equal(true)
  })

  it('should console.log link\'s href when a link is clicked', () => {
    sinon.spy(console, 'log')

    const links = vm.$el.querySelectorAll('.hello a')
    for (let i = 0; i < links.length; i++) {
      links[i].click()
      expect(console.log).to.be.calledWith(vm.$data.links[i].href)
    }

    console.log.restore()
  })

  it('should bind links correct data', () => {
    const links = vm.$el.querySelectorAll('.hello a')
    for (let i = 0; i < links.length; i++) {
      expect(links[i].textContent).to.equal(vm.$data.links[i].text)
      expect(links[i].title).to.equal(vm.$data.links[i].text + ' page')
    }
  })
})
