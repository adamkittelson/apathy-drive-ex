import {Socket} from "phoenix"

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin:abilities", {character: characterID}),
    abilities: {},
    sortBy: "id",
    descending: false,
    page: 1,
    valid_targets: []
  },
  mutations: {
    update_ability(state, form_data) {
      var index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.id
      })
      state.abilities[index].data = form_data
      state.channel.push("update", form_data)
      .receive("ok", function() {
        console.log(form_data.name + " Updated")
        state.abilities[index].dialog = false
      })
    },
    updateName(state, payload) {
      var index = _.findIndex(store.state.abilities, function(item) {
        return item.id === payload.id
      })
      state.abilities[index].name = payload.name
      state.channel.push("update_name", {id: payload.id, name: payload.name})
      .receive("ok", () => console.log("Name Updated") )
    },
    updatePagination(state, payload) {
      state.sortBy = payload.sortBy
      state.descending = payload.descending
      state.page = payload.page
    }
  }
})

window.abilities = new Vue({
  el: "#app",
  store,
  template: '#abilities',
  computed: {
    abilities: function() {
      return Object.values(this.$store.state.abilities)
    }
  },
  methods: {
    updateName: function(id, name) {
      this.$store.commit('updateName', {id: id, name: name})
    },
    updatePageNumber: function(page) {
      this.$store.commit('updatePagination', {page: page, sortBy: this.$store.state.sortBy, descending: this.$store.state.descending})
      this.fetchPage()
    },
    updatePagination: function(pagination) {
      this.$store.commit('updatePagination', {page: this.$store.state.page, sortBy: pagination.sortBy, descending: !!pagination.descending})
      this.fetchPage()
    },
    fetchPage: function() {
      if (this.$store.state.channel.state === "joined") {
        abilities.loading = true
        var args = {query: abilities.search, page_number: this.$store.state.page, order_by: this.$store.state.sortBy, descending: this.$store.state.descending}
        this.$store.state.channel.push("fetch_page", args)
      }
    },
    submit(form_data) {
      if (this.$refs.form.validate()) {
        // Native form submission is not yet supported
        console.log("submitting!")
        this.$store.commit('update_ability', form_data)
      }
    },
    valid_targets: function() {
      return this.$store.state.valid_targets
    }
  },
  data() {
    return {
      pagination: {
        sortBy: 'id'
      },
      nameRules: [
        (v) => !!v || 'Name is required',
        (v) => v && v.length <= 20 || 'Name must be less than 20 characters'
      ],
      search: '',
      loading: true,
      headers: [
        {
          text: 'ID',
          align: 'left',
          value: 'id'
        },
        { text: 'Name', align: 'left', value: 'name' }
      ]
    }
  }
});

store.state.channel.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

store.state.channel.on("abilities", function(data){
  console.log(data)
  abilities.loading = false
  var items = _.map(data.page.entries, function(ability) {
    var item = {
      form: Object.assign({}, ability),
      dialog: false,
      valid: true,
      data: Object.assign({}, ability)
    }
    return item;
  })
  store.state.abilities = items;
  store.state.valid_targets = data.valid_targets;
  abilities.pagination = {
    page: data.page.page_number,
    rowsPerPage: data.page.page_size,
    totalItems: data.page.total_entries,
    totalPages: data.page.total_pages
  }
});
