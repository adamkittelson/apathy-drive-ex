<template>
  <div>
    <em>Change the title of your shopping list here</em>
    <input :value="value" @input="onInput">
  </div>
</template>


<script>
module.exports = {
  template: '#change-title',
  props: ['value'],
  methods: {
    onInput: function(event) {
      this.$emit('input', event.target.value)
    }
  }
}
</script>
