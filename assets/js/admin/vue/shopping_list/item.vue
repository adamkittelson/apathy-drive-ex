<template>
  <li :class="{ 'removed': item.checked}">
    <div class="checkbox">
      <label>
        <input type="checkbox" v-model="item.checked">
        {{ item.text }}
      </label>
    </div>
  </li>
</template>

<script>
module.exports = {
  props: ['item']
}
</script>

<style scoped>
  .container {
    width: 40%;
    margin: 20px auto 0px auto;
  }

  .removed label {
    text-decoration: line-through;
  }

  ul li {
    list-style-type: none;
  }

  ul li span {
    margin-left: 5px;
  }

  .footer {
    font-size: 0.7em;
    margin-top: 20vh;
  }

</style>