<template>
  <div id="shopping-list" class="container">
    <h2>{{ title }}</h2>
    <add-item :items="items"></add-item>
    <items :items="items"></items>
    <div class="footer">
      <hr/>
      <change-title v-model="title"></change-title>
    </div>
  </div>
</template>

<script>
import Item from './shopping_list/item.vue'
import AddItem from './shopping_list/add_item.vue'
import Items from './shopping_list/items.vue'
import ChangeTitle from './shopping_list/change_title.vue'

var data = {
  items: [{ text: 'Bananas', checked: true }, { text: 'Apples', checked: false }],
  title: 'My Shopping List'
};

export default {
  data: function() {
    return data;
  },
  components: { AddItem, ChangeTitle, Item, Items }
};

</script>