export const baseUrl = 'http://localhost:4000';

 async function postJSON(url, data) {
  try {
    let response = await fetch(url, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data)
    });
    return await response.json();
  } catch(err){
    console.error(err);
  }
}

export async function getPosts() {
  try {
    let response = await fetch(baseUrl + '/posts');
    return (await response.json()).data;
  } catch(error) {
    console.error(error);
  }
}

/**
 * @param {String} title
 * @param {String} content
 */
export async function createPost(title, content) {
  try {
    const data = {
      post: {
        title: title,
        content: content
      }
    };
    let response = await postJSON(baseUrl + '/posts', data);
    return response.data;
  } catch (error) {
    console.error(error);
  }
}
