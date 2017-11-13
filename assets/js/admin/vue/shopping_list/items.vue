<template>
  <ul>
    <item v-for="item in items" :item="item" :key="item.text"></item>
  </ul>
</template>

<script>
import Item from './item.vue'

module.exports = {
  template: '#items',
  props: ['items'],
  methods: {
    onInput: function(event) {
      this.$emit('input', event.target.value)
    }
  },
  components: {
    Item
  }
}
</script>
